#include "infowidget.hpp"
#include "ui_infowidget.h"

using namespace gui;

InfoWidget::InfoWidget(QWidget* parent)
    : QWidget(parent), ui(new Ui::InfoWidget) {
    ui->setupUi(this);
}

InfoWidget::~InfoWidget() { delete ui; }
