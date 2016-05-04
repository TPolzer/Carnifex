#pragma once
#include <QObject>
#include <QDateTime>
#include <QTimer>

class Clock : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QDateTime time READ time NOTIFY timeChanged)
    Q_PROPERTY(int interval READ interval WRITE setInterval NOTIFY intervalChanged)
    Q_PROPERTY(int offset READ offset WRITE setOffset NOTIFY offsetChanged)
private:
    QDateTime m_ntime;
    QDateTime m_time;
    int m_offset;
    int m_interval;
    QTimer m_timer;
private slots:
    void update();
    void updateTime();
public:
    Clock(QObject *parent = nullptr);
    QDateTime time() {
        return m_time;
    }
    int interval() {
        return m_interval;
    };
    void setInterval(int i) {
        m_interval = i;
        emit intervalChanged();
    }
    int offset() {
        return m_offset;
    }
    void setOffset(int o) {
        m_offset = o;
        emit offsetChanged();
    }
    Q_INVOKABLE QString formatTime(QDateTime time, QString format) {
        return time.toString(format);
    }
    Q_INVOKABLE QString formatUTCTime(QDateTime time, QString format) {
        return time.toUTC().toString(format);
    }
signals:
    void intervalChanged();
    void timeChanged();
    void offsetChanged();
};
