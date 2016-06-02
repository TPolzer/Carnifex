/*
 * This file is part of "Carnifex"
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
#include <new>
#include <QtEndian>
#include <QMetaEnum>
#include <QQmlComponent>
#include <QVariant>
#include <QString>
#include <QTimer>
#include <sodium.h>
#include <exception>
#include "qmlproto.h"
#include "scoreboard.pb.h"

constexpr static int KEYLEN = 32;
static_assert(crypto_secretbox_KEYBYTES == KEYLEN, "key length disagreement");
constexpr static int NONCELEN = 24;
static_assert(crypto_secretbox_NONCEBYTES == NONCELEN, "nonce length disagreement");
constexpr static int SALTLEN = 32;


ScoreboardClient::ScoreboardClient(const QJsonDocument &config, QQmlEngine &engine)
	:config(config.object()), engine(engine)
{
	auto sharedSecretIt = this->config.find("sharedsecret");
	if(sharedSecretIt == this->config.end()) {
		throw std::runtime_error("no sharedsecret provided");
	}
	sharedSecret = (*sharedSecretIt).toString().toStdString();
	auto serverNameIt = this->config.find("servername");
	if(serverNameIt != this->config.end()) {
		serverName = (*serverNameIt).toString();
	}
	auto serverPortIt = this->config.find("serverport");
	if(serverPortIt != this->config.end()) {
		serverPort = (*serverPortIt).toInt();
	}
	key = std::unique_ptr<unsigned char[]>(new unsigned char[KEYLEN]);
	nonce = std::unique_ptr<unsigned char[]>(new unsigned char[NONCELEN]);
}

void ScoreboardClient::run() {
	emit configure(this->config.toVariantMap());
    QObject::connect(&socket, &QTcpSocket::connected, this, &ScoreboardClient::connected);
	qRegisterMetaType<QAbstractSocket::SocketError>("QAbstractSocket::SocketError");
	socketIsFatal = QObject::connect(&socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(fatal(QAbstractSocket::SocketError)));
	QObject::connect(this, &ScoreboardClient::error, this, &ScoreboardClient::reset);
	QObject::connect(&beatTimer, &QTimer::timeout, this, &ScoreboardClient::reset);
    QObject::connect(&socket, &QIODevice::readyRead, this, &ScoreboardClient::readyRead);
	QObject::connect(&reconnectIdle, &QTimer::timeout, this, &ScoreboardClient::connect);
	reconnectIdle.setSingleShot(true);
    connect();
}

void ScoreboardClient::connect() {
	socket.connectToHost(serverName, serverPort);
}

void ScoreboardClient::reconnect(QAbstractSocket::SocketError) {
	std::cerr << "reconnecting\n";
	if(!reconnectIdle.isActive())
		reconnectIdle.start(2000);
}

void ScoreboardClient::reset() {
	socket.close();
	beatTimer.stop();
	emit reconnect(QAbstractSocket::UnknownSocketError);
}

void ScoreboardClient::connected() {
	std::cerr << "connected\n";
	encrypted = false;
	buffer.resize(NONCELEN-8+SALTLEN);
	nctr = 0;
	expectedBeat = 0;
	beatTimer.setSingleShot(true);
	beatTimer.start(4000);
    pos = begin(buffer);
	if(socketIsFatal) QObject::connect(&socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(reconnect(QAbstractSocket::SocketError)));
	disconnect(socketIsFatal);
}

void ScoreboardClient::readyRead() {
	if(!encrypted) {
		pos += socket.read(&*pos, end(buffer)-pos);
		if(pos == end(buffer)) {
			//salt is only needed temporarily to derive the key
			unsigned char salt[SALTLEN];
			
			std::copy_n(begin(buffer), NONCELEN-8, nonce.get()+8); // nonce is 8 bytes counter . 16 bytes session nonce
			std::copy_n(begin(buffer)+NONCELEN-8, SALTLEN, salt);
			
			if(crypto_pwhash_scryptsalsa208sha256_ll(
					reinterpret_cast<const uchar*>(sharedSecret.c_str()), sharedSecret.size(),
					salt, SALTLEN,
					16384, 8, 1,
					key.get(), 32
			)) {
				throw std::bad_alloc();//OOM
			};
			encrypted = true;
		}
	} else if(pos == end(buffer)) {
		if(socket.bytesAvailable() < qint64(sizeof(packetSize)))
			return;
		buffer.assign(sizeof(packetSize), 0x3f);
		socket.read(&buffer[0], sizeof(packetSize));
        packetSize = qFromBigEndian<qint64>(reinterpret_cast<uchar*>(&buffer[0]));
		if(packetSize > 1024*1024*1024 || packetSize < 0) {
			std::cerr << "Received unplausible message size: " << packetSize << std::endl;
			emit error();
			return;
		}
		buffer.resize(packetSize + crypto_secretbox_MACBYTES);
		pos = begin(buffer);
	} else {
		pos += socket.read(&*pos, end(buffer)-pos);
		if(pos == end(buffer)) {
			qToBigEndian(nctr, nonce.get());
			++nctr;
			std::vector<char> decrypted(packetSize);
			auto cipherText = reinterpret_cast<unsigned char*>(&buffer[0]);
			auto plainText = reinterpret_cast<unsigned char*>(&decrypted[0]);
			int fail = crypto_secretbox_open_easy(
					plainText, cipherText,
					buffer.size(), nonce.get(), key.get()
			);
			if(fail) {
				std::cerr << "decryption of packet " << nctr-1 << " failed" << std::endl;
				emit error();
				return;
			}
            m.ParseFromArray(&decrypted[0], packetSize);
			if(!m.IsInitialized()) {
				std::cerr << "Received inconsistent message (" << packetSize << " bytes): " << m.InitializationErrorString() << std::endl;
				emit error();
				return;
			}
			if(m.has_event()) {
				applyEvent(m.event());
			} else if(m.has_setup()) {
				setup(m.setup());
			} else if(m.has_heartbeat()) {
				if(m.heartbeat() == expectedBeat) {
					expectedBeat++;
					beatTimer.start(); // rearm
				}
			} else if(m.has_unfreeze()) {
				if(m.unfreeze()) unfreeze();
				else refreeze();
			} else {
				std::cerr << "Received empty message (Should never happen!)" << std::endl;
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
	auto name = QString::fromStdString(setup.name());
	auto configName = this->config.find("name");
	if(configName != this->config.end()) {
		name = (*configName).toString();
	}
	double start = setup.start() * 1000.0;
	double freeze = setup.freeze() * 1000.0;
	double end = setup.end() * 1000.0;
	auto teams = setup.teams();
	sort(teams.begin(), teams.end(), [](const wire::Team& a, const wire::Team& b){
			return a.name() < b.name();
			});
	auto problems = setup.problems();
	auto categories = setup.categories();
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
		auto affiliation = QString::fromStdString(team.affiliation());
		auto id = team.id();
		this->teams[id] = qmlTeam;
		qmlTeam->setProperty("name", name);
		qmlTeam->setProperty("affiliation", affiliation);
		qmlTeam->setProperty("pos", teamList.size());
		auto category = std::find_if(categories.begin(), categories.end(), [&](const wire::Category &c) {
				return c.categoryid() == team.category();
		});
		if(category == categories.end()) {
			std::cerr << "team without category, ignoring: " << team.name() << std::endl;
			delete qmlTeam;
			continue;
		} else {
			qmlTeam->setProperty("teamColor", QString::fromStdString(category->color()));
			qmlTeam->setProperty("sortOrder", (qint64)category->sortorder());
		}
		teamList.push_back(QVariant::fromValue(qmlTeam));
	}
	for(const auto& t : teams) {
		ranking.push_back(this->teams[t.id()]);
	}
	rerank(ranking.end());
	for(const auto& problem : problems) {
		this->problems[problem.id()] = problemList.size();
		problemList.push_back(QString::fromStdString(problem.label()));
	}
	rs.resolvedProblems = problems.size()+1;
	rs.resolvingProblem = -1;
	rs.resolvedTeams = -1;
	refocus();
	resolveStack.clear();
	QVariantMap contest;
	contest["name"] = name;
	contest["start"] = start;
	contest["freeze"] = freeze;
	contest["end"] = end;
	contest["sstart"] = setup.simulatedstart() * 1000.0;
	contest["sspeed"] = setup.simulationspeed();
	emit contestSetup(contest, QVariant(problemList), teamList);

	for(QObject *team : ranking) {
		QQmlEngine::setObjectOwnership(team, QQmlEngine::JavaScriptOwnership);
	}
	std::cerr << "Received setup for Contest \"" << setup.name() << "\"" <<std::endl;
}

bool ScoreboardClient::sortScore(QObject *a, QObject *b) {
    QVariant res;
    QMetaObject::invokeMethod(a, "sortBefore", Q_RETURN_ARG(QVariant, res), Q_ARG(QVariant, QVariant::fromValue(b)));
    return res.toBool();
}

bool ScoreboardClient::compareScore(QObject *a, QObject *b) {
    QVariant res;
    QMetaObject::invokeMethod(a, "betterThan", Q_RETURN_ARG(QVariant, res), Q_ARG(QVariant, QVariant::fromValue(b)));
    return res.toBool();
}

void ScoreboardClient::applyEvent(const wire::Event& event) {
	auto team = teams[event.team()];
	auto problem = problems.find(event.problem());
	if(problem == problems.end())
		return;
	QVariantMap jEvent = messageToObject(event);
	QMetaObject::invokeMethod(team, "applyEvent", Q_ARG(QVariant, jEvent), Q_ARG(QVariant, problem->second));
	rerank(std::find(ranking.begin(), ranking.end(), team));
}
	
void ScoreboardClient::rerank(std::vector<QObject*>::iterator changed) {
	if(changed == ranking.end()) {
		std::stable_sort(std::begin(ranking), std::end(ranking), &sortScore);
	} else {
		std::inplace_merge(std::begin(ranking), changed, changed+1, &sortScore);
		std::inplace_merge(changed, changed+1, std::end(ranking), &sortScore);
	}
	if(ranking.empty()) return;
	ranking[0]->setProperty("pos", 0);
	ranking[0]->setProperty("rank", 1);
	for(quint64 pos = 1; pos < ranking.size(); ++pos) {
		QMetaObject::invokeMethod(ranking[pos], "rankAfter", Q_ARG(QVariant, QVariant::fromValue(ranking[pos-1])));
	}
}

void ScoreboardClient::unfreeze() {
	resolveStack.emplace_back(rs, nullptr, ranking);
	if(rs.resolvedTeams >= int(ranking.size())) {
		++rs.resolvedTeams;
		return;
	}
	auto teamIt = ranking.rbegin() + rs.resolvedTeams;
	QObject *team = *teamIt;
	if(rs.resolvingProblem > rs.resolvedProblems-1) { // toggle one problem
		std::get<1>(*resolveStack.rbegin()) = team;
		QMetaObject::invokeMethod(team, "toggleFreeze", Q_ARG(QVariant, int(problems.size() - rs.resolvingProblem - 1)));
		rs.resolvedProblems = rs.resolvingProblem+1;
		rerank(teamIt.base()-1);
		if(*teamIt != team) { // team has moved up, focus other team
			rs.resolvedProblems = 0;
			rs.resolvingProblem = -1;
		}
	} else if(rs.resolvedProblems < int(problems.size())) { // highlight next problem
		++rs.resolvingProblem;
		auto l = team->property("pending").toList();
		while(rs.resolvingProblem < int(problems.size()) && l.value(problems.size()-rs.resolvingProblem-1, 0).toInt() == 0) {
			++rs.resolvingProblem;
		}
		if(rs.resolvingProblem == int(problems.size())) {
			rs.resolvedProblems = problems.size();
		} else {
		}
	}
	if(rs.resolvedProblems == int(problems.size())) { //highlight team
		rs.resolvingProblem = -1;
		++rs.resolvedProblems;
	} else if(rs.resolvedProblems == int(problems.size())+1) { // highlight next team
		++rs.resolvedTeams;
		rs.resolvedProblems = 0;
		rs.resolvingProblem = -1;
	}
	refocus();
}

void ScoreboardClient::refreeze() {
	QObject *team;
	std::tie(rs, team, ranking) = *resolveStack.rbegin();
	if(team) {
		QMetaObject::invokeMethod(team, "toggleFreeze", Q_ARG(QVariant, int(problems.size() - rs.resolvingProblem - 1)));
	}
	resolveStack.pop_back();
	refocus();
	rerank(ranking.begin());
}

void ScoreboardClient::refocus() {
	focus(QPoint(problems.size()-rs.resolvingProblem-1, teams.size()-rs.resolvedTeams-1));
}

void ScoreboardClient::fatal(QAbstractSocket::SocketError error) {
	const QMetaObject &mo = QAbstractSocket::staticMetaObject;
	int index = mo.indexOfEnumerator("SocketError");
	QMetaEnum metaEnum = mo.enumerator(index);
	std::cerr << "fatal: " << metaEnum.valueToKey(error) << std::endl;
	abort();
}
