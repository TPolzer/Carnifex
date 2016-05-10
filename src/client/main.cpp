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
#include <QGuiApplication>
#include <QQmlApplicationEngine>
#include <QtQml>
#include <QQuickWindow>
#include <QFile>
#include <iostream>
#include <sodium.h>
#include "ScoreboardClient.h"
#include "clock.h"

int main(int argc, char *argv[])
{
	if(sodium_init()) {
		std::cerr << "is /dev/urandom accessible?\n";
		abort();
	}
    QGuiApplication app(argc, argv);

	QFile configFile("config.json");
	configFile.open(QIODevice::ReadOnly | QIODevice::Text);
	QJsonDocument config = QJsonDocument::fromJson(configFile.readAll());


    QQmlApplicationEngine engine;

	qmlRegisterType<Clock>("de.bulsa.clock", 0, 1, "Clock");

    engine.load(QUrl(QStringLiteral("qrc:/main.qml")));

    QQuickWindow *obj = qobject_cast<QQuickWindow*>(engine.rootObjects().first());
	ScoreboardClient client(config, engine);
	
	QObject::connect(&client, SIGNAL(contestSetup(QVariant,QVariant,QVariant)),
			obj, SLOT(contestSetup(QVariant,QVariant,QVariant)));
	QObject::connect(&client, SIGNAL(configure(QVariant)),
			obj, SLOT(configure(QVariant)));
	QObject::connect(&client, SIGNAL(focus(QVariant)), obj, SLOT(doFocus(QVariant)));
	
	client.run();



    return app.exec();
}
