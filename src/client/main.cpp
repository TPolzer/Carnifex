#include <QGuiApplication>
#include <QQmlApplicationEngine>
#include <QQuickWindow>
#include <QVariant>
#include <QMetaObject>
#include <QJsonArray>
#include <QTcpSocket>
#include <QtEndian>
#include "ScoreboardClient.h"

int main(int argc, char *argv[])
{
    QGuiApplication app(argc, argv);

    QQmlApplicationEngine engine;

    engine.load(QUrl(QStringLiteral("qrc:/main.qml")));

    QQuickWindow *obj = qobject_cast<QQuickWindow*>(engine.rootObjects().first());
	ScoreboardClient client(engine);
	client.run();

	QObject::connect(&client, SIGNAL(contestSetup(QVariant,QVariant,QVariant)),
			obj, SLOT(contestSetup(QVariant,QVariant,QVariant)));


    return app.exec();
}
