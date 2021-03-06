#pragma once

#include <QWidget>

namespace Ui {
    class InfoWidget;
}

namespace gui {

//! the info pane on the right side of the UI
class InfoWidget : public QWidget {
    Q_OBJECT

public:
    explicit InfoWidget(QWidget* parent = 0);
    ~InfoWidget();

private:
    Ui::InfoWidget* ui;
};

}