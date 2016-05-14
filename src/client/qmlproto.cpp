/*
 * This file is part of "Carnifex"
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
		} else if(field->type() == FieldDescriptor::TYPE_DOUBLE) {
			res[name] = refl->GetDouble(m, field);
		} else if(field->type() == FieldDescriptor::TYPE_MESSAGE) {
            res[name] = messageToObject(refl->GetMessage(m, field));
        } else {
            std::cerr << "Trying to pass unsupported proto object to js\n";
            abort();
        }
	}
	return res;
}
