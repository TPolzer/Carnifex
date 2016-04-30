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
                Tsubmits[problem] = QVariant(qint64(submitCount));
                Tpending[problem] = QVariant(state == wire::PENDING);
                Tcorrect[problem] = QVariant(state == wire::CORRECT);
                team->setProperty("submits", Tsubmits);
                team->setProperty("pending", Tpending);
                team->setProperty("correct", Tcorrect);
                team->setProperty("penalty", qint64(penalty));
                std::vector<QObject*> ranking;
                ranking.reserve(teams.size());
                for(const auto& t : teams) {
                    ranking.push_back(t.second);
                }
                auto comp = [&](QObject *a, QObject *b){
                    auto sd = a->property("solved").toInt() - b->property("solved").toInt();
                    auto pd = a->property("penalty").toInt() - b->property("penalty").toInt();
                    if(sd > 0) return true;
                    else if(sd == 0) return pd < sd;
                    return false;
                };
                sort(begin(ranking),end(ranking),comp);
                int rank = 1;
                int pos = 0;
                for(auto it = begin(ranking); it != end(ranking); ++it) {
                    if(it == begin(ranking) || comp(*std::prev(it), *it))
                        (*it)->setProperty("rank", rank++);
					else {
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
					QUrl::fromLocalFile("Team.qml"));
				QVariantList teamList;
				QVariantList problemList;
				this->teams.clear();
				this->problems.clear();
				for(const auto& team : teams) {
					QObject *qmlTeam = teamComponent.create();
					auto name = QString::fromStdString(team.name());
					auto id = team.id();
					this->teams[id] = qmlTeam;
					qmlTeam->setProperty("name", name);
					qmlTeam->setProperty("penalty", QVariant(0));
					qmlTeam->setProperty("rank", 1);
					qmlTeam->setProperty("pos", teamList.size());
					QVariantList correct;
					QVariantList submits;
					QVariantList pending;
					for(auto l : {&correct, &submits, &pending}) {
						for(const auto& problem : problems) {
							l->append(0);
						}
					}
					qmlTeam->setProperty("correct", correct);
					qmlTeam->setProperty("submits", submits);
					qmlTeam->setProperty("pending", pending);
					teamList.push_back(QVariant::fromValue(qmlTeam));
				}
				for(const auto& problem : problems) {
					this->problems[problem.id()] = problemList.size();
					problemList.push_back(QString::fromStdString(problem.label()));
				}
				emit contestSetup(QVariant(problemList), teamList);
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
