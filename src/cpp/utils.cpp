
#include "QtGui"

void error(QString msg) {
    qDebug() << "ERROR (in cpp-land):" << msg;
    exit(1);
};

bool isArrowKey(QKeyEvent* e) {
    int k = e->key();
    return (k == Qt::Key_Left || k == Qt::Key_Right || k == Qt::Key_Up || k == Qt::Key_Down);
};
