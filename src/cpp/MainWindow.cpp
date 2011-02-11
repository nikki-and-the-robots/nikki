
#include "utils.h"
#include "MainWindow.h"

MainWindow::MainWindow() {
};

void MainWindow::setChild(GLContext* child) {
    this->child = child;
    QVBoxLayout* layout = new QVBoxLayout();
    layout->setContentsMargins(QMargins());
    layout->addWidget(child);
    this->setLayout(layout);
};
void MainWindow::keyPressEvent(QKeyEvent* e) {
    child->keyPressEvent(e);
};


void MainWindow::keyReleaseEvent(QKeyEvent* e) {
    child->keyReleaseEvent(e);
};

void MainWindow::closeEvent(QCloseEvent* e) {
    child->closeEvent(e);
};
