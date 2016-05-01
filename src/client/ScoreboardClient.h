#pragma once
#include <QThread>
#include <QVariant>
#include <QTcpSocket>
#include <vector>
#include <QQmlEngine>
#include "scoreboard.pb.h"

class ScoreboardClient : public QObject
{
	Q_OBJECT
private:
	QString serverName = "localhost";
	quint16 serverPort = 8080;
	QTcpSocket socket;
	QMetaObject::Connection socketIsFatal;
	std::vector<char> buffer;
	decltype(begin(buffer)) pos;
	qint64 packetSize;
	wire::Message m;
	QQmlEngine& engine;
	std::map<qint64, QObject*> teams;
	std::map<qint64, qint64> problems; // id -> idx
public:
	ScoreboardClient(QQmlEngine& engine);
signals:
	void contestSetup(QVariant contest, QVariant problems, QVariant teams);
	void event(QVariant event);
	void error();
public slots:
	void connected();
	void fatal(QAbstractSocket::SocketError);
	void readyRead();
	void run();
};
