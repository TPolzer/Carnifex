import QtQuick 2.0
import "helper.js" as H

Item {
    property var name: ""
    property var penalty: 0
    property int rank: 0
    property int pos: 0
    property var submits: []
    property var pending: []
    property var correct: []
    property var solved: correct.reduce(H.add, 0)
}
