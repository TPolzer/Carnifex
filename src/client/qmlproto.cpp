#include "qmlproto.h"
#include <iostream>

using namespace google::protobuf;

QVariantMap messageToObject(const Message& m) {
	QVariantMap res;
	auto desc = m.GetDescriptor();
	auto refl = m.GetReflection();
	for(int i = 0; i < desc->field_count(); ++i) {
		auto field = desc->field(i);
        if(!refl->HasField(m, field)) continue;
		QString name = QString::fromStdString(field->name());
		if(field->type() == FieldDescriptor::TYPE_INT64) {
			res[name] = qint64(refl->GetInt64(m, field));
		} else if(field->type() == FieldDescriptor::TYPE_ENUM) {
			res[name] = QString::fromStdString(refl->GetEnum(m, field)->name());
		} else if(field->type() == FieldDescriptor::TYPE_MESSAGE) {
            res[name] = messageToObject(refl->GetMessage(m, field));
        } else {
            std::cerr << "Trying to pass unsupported proto object to js\n";
            abort();
        }
	}
	return res;
}
