#pragma once
#include <QVariantMap>
#include <google/protobuf/message.h>

QVariantMap messageToObject(const google::protobuf::Message& m);
