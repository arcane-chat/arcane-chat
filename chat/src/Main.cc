#include <QApplication>
#include "PureScript/PureScript.hh"
#include <QMessageBox>
#include "../src/client.hh"

namespace Main {
  using namespace PureScript;
  auto uiApp(const any& f) -> any {
    return [=]() -> any {
      int argc = 1;
      char *argv[] = { "arcane-chat", nullptr };
      return old_main(argc, argv);
      /*QApplication app(argc, nullptr);
      QMessageBox msgBox;
      msgBox.setText("This is a test");
      msgBox.setStandardButtons(QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
      msgBox.setDefaultButton(QMessageBox::Save);

      // Use Qt signals/slots to attach lambda which calls PureScript eff function
      //
      QObject::connect( &msgBox, &QMessageBox::finished, [=](int result) { f(result)(); } );

      msgBox.show();
      return app.exec();*/
    };
  }
}
