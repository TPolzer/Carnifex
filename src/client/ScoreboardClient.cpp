/*
 * This file is part of "The Scoreboard"
 * Copyright (C) 2016  Tobias Polzer

 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
#include "ScoreboardClient.h"
#include <iostream>
#include <vector>
#include <QtEndian>
#include <QMetaEnum>
#include <QQmlComponent>
#include <QVariant>
#include <QTimer>
#include "qmlproto.h"
#include "scoreboard.pb.h"


ScoreboardClient::ScoreboardClient(QQmlEngine& engine)
	:buffer(8), engine(engine), pos(end(buffer))
{
}

void ScoreboardClient::run() {
    QObject::connect(&socket, &QTcpSocket::connected, this, &ScoreboardClient::connected);
	qRegisterMetaType<QAbstractSocket::SocketError>("QAbstractSocket::SocketError");
	socketIsFatal = QObject::connect(&socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(fatal(QAbstractSocket::SocketError)));
	QObject::connect(this, &ScoreboardClient::error, this, &ScoreboardClient::reset);
    connect();
}

void ScoreboardClient::connect() {
    QObject::connect(&socket, &QIODevice::readyRead, this, &ScoreboardClient::readyRead);
	socket.connectToHost(serverName, serverPort);
}

void ScoreboardClient::reconnect(QAbstractSocket::SocketError) {
	std::cerr << "reconnecting\n";
    QTimer::singleShot(2000, this, &ScoreboardClient::connect);
}

void ScoreboardClient::reset() {
	socket.close();
	emit reconnect(QAbstractSocket::UnknownSocketError);
}

void ScoreboardClient::connected() {
	std::cerr << "connected\n";
    pos = end(buffer);
	if(socketIsFatal) QObject::connect(&socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(reconnect(QAbstractSocket::SocketError)));
	disconnect(socketIsFatal);
}

void ScoreboardClient::readyRead() {
	if(pos == end(buffer)) {
		if(socket.bytesAvailable() < sizeof(packetSize))
			return;
		buffer.assign(sizeof(packetSize), 0x3f);
		socket.read(&buffer[0], sizeof(packetSize));
        packetSize = qFromBigEndian<qint64>(reinterpret_cast<uchar*>(&buffer[0]));
		if(packetSize > 1024*1024*1024 || packetSize < 0) {
			std::cerr << "Received unplausible message size: " << packetSize << std::endl;
			emit error();
			return;
		}
		buffer.resize(packetSize);
		pos = begin(buffer);
	} else {
		pos += socket.read(&*pos, end(buffer)-pos);
		if(pos == end(buffer)) {
            m.ParseFromArray(&buffer[0], packetSize);
			if(!m.IsInitialized()) {
				std::cerr << "Received inconsistent message (#1)" << std::endl;
				emit error();
				return;
			}
			if(m.has_event()) {
				applyEvent(m.event());
			} else if(m.has_setup()) {
				setup(m.setup());
			} else {
				std::cerr << "Received inconsistent message (#2)" << std::endl;
				emit error();
			}
		}
	}
	if(socket.bytesAvailable())
		readyRead();
}

void ScoreboardClient::setup(const wire::ContestSetup& setup) {
	this->teams.clear();
	this->problems.clear();
	this->ranking.clear();
	this->pendingFreeze.clear();
	auto name = QString::fromStdString(setup.name());
	double start = setup.start() * 1000.0;
	auto teams = setup.teams();
	sort(teams.begin(), teams.end(), [](const wire::Team& a, const wire::Team& b){
			return a.name() < b.name();
			});
	auto problems = setup.problems();
	QQmlComponent teamComponent(&engine,
			QUrl(QStringLiteral("qrc:/Team.qml")));
	QVariantList teamList;
	QVariantList problemList;
	std::sort(problems.begin(), problems.end(), [](const wire::Problem& a, const wire::Problem& b){
			return a.label() < b.label();
			});
	for(const auto& team : teams) {
		QObject *qmlTeam = teamComponent.create();
		auto name = QString::fromStdString(team.name());
		auto id = team.id();
		this->teams[id] = qmlTeam;
		qmlTeam->setProperty("name", name);
		qmlTeam->setProperty("pos", teamList.size());
		teamList.push_back(QVariant::fromValue(qmlTeam));
	}
	for(const auto& t : teams) {
		ranking.push_back(this->teams[t.id()]);
	}
	for(const auto& problem : problems) {
		this->problems[problem.id()] = problemList.size();
		problemList.push_back(QString::fromStdString(problem.label()));
	}
	QVariantMap contest;
	contest["name"] = name;
	contest["start"] = start;
	emit contestSetup(contest, QVariant(problemList), teamList);

	//TODO is this correct / sufficient / necessary?
	QQmlEngine::setObjectOwnership(qvariant_cast<QObject *>(teamList), QQmlEngine::JavaScriptOwnership);
	QQmlEngine::setObjectOwnership(qvariant_cast<QObject *>(problemList), QQmlEngine::JavaScriptOwnership);
	std::cerr << "Received setup for Contest \"" << setup.name() << "\"" <<std::endl;
}

bool ScoreboardClient::compareScore(QObject *a, QObject *b) {
    QVariant res;
    QMetaObject::invokeMethod(a, "betterThan", Q_RETURN_ARG(QVariant, res), Q_ARG(QVariant, QVariant::fromValue(b)));
    return res.toBool();
}

void ScoreboardClient::applyEvent(const wire::Event& event) {
	auto team = teams[event.team()];
	auto problem = problems[event.problem()];
	if(event.has_unfrozen()) {
		pendingFreeze[team][problem] = event.unfrozen();
	}
	QVariantMap jEvent = messageToObject(event);
	QMetaObject::invokeMethod(team, "applyEvent", Q_ARG(QVariant, jEvent), Q_ARG(QVariant, problem));
	std::stable_sort(std::begin(ranking),std::end(ranking),&compareScore);
    int rank = 0;
    for(quint64 pos = 0; pos < ranking.size(); ++pos) {
        ranking[pos]->setProperty("pos", pos);
        if(pos == 0 || compareScore(ranking[pos-1], ranking[pos]))
            rank = pos+1;
        ranking[pos]->setProperty("rank", rank);
    }
}

void ScoreboardClient::fatal(QAbstractSocket::SocketError error) {
	std::cerr << "fatal: " << QMetaEnum::fromType<QAbstractSocket::SocketError>().valueToKey(error) << std::endl;
    abort();
}
