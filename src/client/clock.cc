#include "clock.h"

Clock::Clock(QObject *parent) 
    :QObject(parent), m_offset(0), m_interval(1000)
{
    m_timer.setTimerType(Qt::PreciseTimer);
    m_timer.setSingleShot(true);
    connect(&m_timer, &QTimer::timeout, this, &Clock::updateTime);
    connect(this, &Clock::intervalChanged, this, &Clock::update);
    connect(this, &Clock::offsetChanged, this, &Clock::update);
    update();
}

void Clock::update() {
    auto now = QDateTime::currentDateTime().toMSecsSinceEpoch();
    now -= now % m_interval;
    now += m_offset % m_interval;
    m_ntime = QDateTime::fromMSecsSinceEpoch(now);
    updateTime();
}

void Clock::updateTime() {
    m_time = m_ntime;
    emit timeChanged();
    auto diff = QDateTime::currentDateTime().msecsTo(m_ntime);
    while(diff <= 0) {
        m_ntime = m_ntime.addMSecs(m_interval);
        diff = QDateTime::currentDateTime().msecsTo(m_ntime);
    }
    m_timer.setInterval(diff);
    m_timer.start();
}
