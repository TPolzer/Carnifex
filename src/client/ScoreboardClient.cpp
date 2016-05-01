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
#include "scoreboard.pb.h"


ScoreboardClient::ScoreboardClient(QQmlEngine& engine)
	:buffer(8), engine(engine), pos(end(buffer))
{
}

void ScoreboardClient::run() {
	connect(&socket, &QTcpSocket::connected, this, &ScoreboardClient::connected);
	qRegisterMetaType<QAbstractSocket::SocketError>("QAbstractSocket::SocketError");
	socketIsFatal = connect(&socket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(fatal(QAbstractSocket::SocketError)));
	connect(&socket, &QIODevice::readyRead, this, &ScoreboardClient::readyRead);
	socket.connectToHost(serverName, serverPort);
}

void ScoreboardClient::connected() {
	std::cerr << "connected\n";
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
                auto event = m.event();
                auto team = teams[event.team()];
                auto problem = problems[event.problem()];
                auto submitCount = event.submitcount();
                auto penalty = event.penalty();
                auto state = event.state();
                auto Tsubmits = team->property("submits").toList();
                auto Tpending = team->property("pending").toList();
                auto Tcorrect = team->property("correct").toList();
                auto Tpenalties = team->property("penalties").toList();
                auto Tfirst = team->property("first").toList();
                Tsubmits[problem] = QVariant(qint64(submitCount));
                Tpending[problem] = QVariant(state == wire::PENDING);
				Tcorrect[problem] = QVariant(state == wire::CORRECT || state == wire::FIRST);
				Tfirst[problem] = QVariant(state == wire::FIRST);
                Tpenalties[problem] = QVariant(qint64(penalty)/60);//TODO: correct rounding?
                team->setProperty("submits", Tsubmits);
                team->setProperty("pending", Tpending);
                team->setProperty("correct", Tcorrect);
                team->setProperty("penalties", Tpenalties);
                team->setProperty("first", Tfirst);
                std::vector<QObject*> ranking;
                ranking.reserve(teams.size());
                for(const auto& t : teams) {
                    ranking.push_back(t.second);
                }
                auto comp = [&](QObject *a, QObject *b){
                    auto sd = a->property("solved").toInt() - b->property("solved").toInt();
					if(sd < 0) return false;
					if(sd > 0) return true;
                    auto pd = a->property("penalty").toInt() - b->property("penalty").toInt();
					if(pd < 0) return true;
					auto fd = a->property("firsts").toInt() - b->property("firsts").toInt();
					if(fd > 0) return true;
                    return false;
                };
                sort(std::begin(ranking),std::end(ranking),comp);
                int rank = 1;
                int pos = 0;
                for(auto it = begin(ranking); it != end(ranking); ++it) {
                    if(it == begin(ranking) || comp(*std::prev(it), *it)) {
                        (*it)->setProperty("rank", rank++);
					} else {
                        (*it)->setProperty("rank", "");
					}
                    (*it)->setProperty("pos", pos++);
                }
			} else if(m.has_setup()) {
				auto setup = m.setup();
				auto name = QString::fromStdString(setup.name());
				auto teams = setup.teams();
				auto problems = setup.problems();
				QQmlComponent teamComponent(&engine,
					QUrl(QStringLiteral("qrc:/Team.qml")));
				QVariantList teamList;
				QVariantList problemList;
				this->teams.clear();
				this->problems.clear();
				std::sort(problems.begin(), problems.end(), [](const auto& a, const auto& b){
					return a.label() < b.label();
				});
				for(const auto& team : teams) {
					QObject *qmlTeam = teamComponent.create();
					auto name = QString::fromStdString(team.name());
					auto id = team.id();
					this->teams[id] = qmlTeam;
					qmlTeam->setProperty("name", name);
					qmlTeam->setProperty("pos", teamList.size());
					QVariantList empty;
					for(const auto& problem : problems) {
						empty.append(0);
					}
					qmlTeam->setProperty("correct", empty);
					qmlTeam->setProperty("submits", empty);
					qmlTeam->setProperty("pending", empty);
					qmlTeam->setProperty("penalties", empty);
					qmlTeam->setProperty("first", empty);
					teamList.push_back(QVariant::fromValue(qmlTeam));
				}
				for(const auto& problem : problems) {
					this->problems[problem.id()] = problemList.size();
					problemList.push_back(QString::fromStdString(problem.label()));
				}
				QVariantMap contest;
				contest["name"] = name;
				emit contestSetup(contest, QVariant(problemList), teamList);
				QQmlEngine::setObjectOwnership(qvariant_cast<QObject *>(teamList), QQmlEngine::JavaScriptOwnership);
				QQmlEngine::setObjectOwnership(qvariant_cast<QObject *>(problemList), QQmlEngine::JavaScriptOwnership);
				std::cerr << "Received setup for Contest \"" << setup.name() << "\"" <<std::endl;
			} else {
				std::cerr << "Received inconsistent message (#2)" << std::endl;
				emit error();
			}
		}
	}
	if(socket.bytesAvailable())
		readyRead();
}

void ScoreboardClient::fatal(QAbstractSocket::SocketError error) {
	std::cerr << "fatal: " << QMetaEnum::fromType<decltype(error)>().valueToKey(error) << std::endl;
	emit this->error();
}

/*       
QString serverName = "localhost";
         quint16 serverPort = 8080;
         QTcpSocket socket;
         std::this_thread::sleep_for(1s);
         socket.connectToHost(serverName, serverPort);
         if(socket.waitForConnected()) {
            exit(1);
         }
         while (true) {
            while (socket.bytesAvailable() < (int)sizeof(int64_t)) {
                if (!socket.waitForReadyRead()) {
                    exit(1);
                }
            }
            qint64 packetSize;
            socket.read(reinterpret_cast<char*>(&packetSize), sizeof(packetSize));
            packetSize = qFromBigEndian(packetSize);
            std::cerr << "read " << packetSize << std::endl;
            wire::Message m;
            while (socket.bytesAvailable() < packetSize) {
                if (!socket.waitForReadyRead()) {
                    exit(1);
                }
            }
            std::vector<char> bytes(packetSize);
            socket.read(&bytes[0], packetSize);
            m.ParseFromArray(&bytes[0], packetSize);
        }
         //QMetaObject::invokeMethod(obj, "contestSetup", Q_ARG(QVariant, QJsonArray()), Q_ARG(QVariant, QJsonArray()));
    });
*/
