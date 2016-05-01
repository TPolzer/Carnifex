import QtQuick 2.0
import "helper.js" as H

Item {
    property var name: ""
    property var rank: ""
    property int pos: 0
    property var submits: []
    property var pending: []
    property var correct: []
    property var penalties: []
    property var first: []
    property var solved: correct.reduce(H.add, 0)
    property int penalty: penalties.reduce(H.add, 0)
    property int firsts: first.reduce(H.add, 0)
}
