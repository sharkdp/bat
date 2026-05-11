import QtQuick 2.0
import "../components"

Page {
    id: page

    // properties

    property bool startup: true
    readonly property var var1: null
    readonly property QtObject var2: null

    allowedOrientations: Orientation.All

    /* components */

    DBusServiceWatcher {
        id: dbusService
        service: "org.bat.service"

        onRegisteredChanged: {
            if (dbusService.registered) {
                announcedNameField.text = daemon.announcedName()
            }
        }
    }

    Component.onCompleted: {
        console.debug("completed")
    }

    Flickable {
        anchors.fill: parent
        contentHeight: column.height
        visible: dbusService.registered

        ViewPlaceholder {
            enabled: !startup
                     && trustedDevices.count == 0
                     && nearDevices.count == 0
            text: qsTr("Install Bat.")
        }

        Column {
            id: column

            width: page.width
            spacing: Theme.paddingLarge

            PageHeader {
                title: qsTr("Syntax Test")
            }

            TextField {
                id: announcedNameField
                width: parent.width
                label: qsTr("Device Name")
                text: dbusService.registered ? daemon.announcedName() : ""

                onActiveFocusChanged: {
                    if (activeFocus)
                        return

                    if (text.length === 0) {
                        text = daemon.announcedName()
                    } else {
                        daemon.setAnnouncedName(text)
                        placeholderText = text
                    }
                }

                EnterKey.onClicked: announcedNameField.focus = false
                EnterKey.iconSource: "image://theme/icon-m-enter-close"
            }


            Component {
                id: deviceDelegate

                ListItem {
                    id: listItem

                    property bool showStatus: deviceStatusLabel.text.length

                    width: page.width
                    height: Theme.itemSizeMedium

                    Image {
                        id: icon
                        source: iconUrl

                        x: Theme.horizontalPageMargin
                        anchors.verticalCenter: parent.verticalCenter
                        sourceSize.width: Theme.iconSizeMedium
                        sourceSize.height: Theme.iconSizeMedium
                    }

                    Label {
                        id: deviceNameLabel
                        anchors {
                            left: icon.right
                            leftMargin: Theme.paddingLarge
                            right: parent.right
                            rightMargin: Theme.horizontalPageMargin
                        }
                        y: listItem.contentHeight / 2 - implicitHeight / 2
                           - showStatus * (deviceStatusLabel.implicitHeight / 2)

                        text: name
                        color: listItem.highlighted
                               ? Theme.highlightColor
                               : Theme.primaryColor
                        truncationMode: TruncationMode.Fade
                        textFormat: Text.PlainText

                        Behavior on y { NumberAnimation {} }
                    }

                    Label {
                        id: deviceStatusLabel
                        anchors {
                            left: deviceNameLabel.left
                            top: deviceNameLabel.bottom
                            right: parent.right
                            rightMargin: Theme.horizontalPageMargin
                        }

                        text: (trusted && reachable)
                              ? qsTr("Connected")
                              : (hasPairingRequests || waitsForPairing
                                 ? qsTr("Pending pairing request ...") : "")
                        color: listItem.highlighted
                               ? Theme.secondaryHighlightColor
                               : Theme.secondaryColor
                        truncationMode: TruncationMode.Fade
                        font.pixelSize: Theme.fontSizeExtraSmall
                        opacity: showStatus ? 1.0 : 0.0
                        width: parent.width
                        textFormat: Text.PlainText

                        Behavior on opacity { FadeAnimation {} }
                    }

                    onClicked: {
                        pageStack.push(
                            Qt.resolvedUrl("DevicePage.qml"),
                            { deviceId: id })
                    }
                }
            }

            DeviceListModel {
                id: devicelistModel
            }

            ColumnView {
                id: devicesView
                width: page.width
                itemHeight: Theme.itemSizeMedium


                model: trustedDevicesModel
                delegate: deviceDelegate
                visible: devicesView.count > 0
            }
        }

        PullDownMenu {
//            MenuItem {
//                text: qsTr("About ...")
//                onClicked: pageStack.push(Qt.resolvedUrl("AboutPage.qml"))
//            }

            MenuItem {
                text: qsTr("Settings ...")
                onClicked: pageStack.push(Qt.resolvedUrl("SettingsPage.qml"))
            }
        }

        VerticalScrollDecorator {}
    }

    /*
    Connections {
        target: ui
        onOpeningDevicePage: openDevicePage(deviceId)
    }*/

    Timer {
        interval: 1000
        running: true
        repeat: false
        onTriggered: startup = false
    }

    function openDevicePage(deviceId) {
        if (typeof pageStack === "undefined")
            return;

        console.log("opening device " + deviceId)

        window.activate()

        var devicePage = pageStack.find(function(page) {
            return page.objectName === "DevicePage"
        })
        if (devicePage !== null && devicePage.deviceId === deviceId) {
            pageStack.pop(devicePage)
            ui.showMainWindow()
            return
        }

        pageStack.pop(page, PageStackAction.Immediate)
        pageStack.push(
            Qt.resolvedUrl("DevicePage.qml"),
            { deviceId: deviceId },
            PageStackAction.Immediate)
    }
}
