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
