/*
  CDE - C/C++ development environment for emacs
  Copyright (C) 2016 Oleg Keri

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>
*/

#define BOOST_TEST_MODULE TestExternal

#include <sys/stat.h>
#include <boost/test/unit_test.hpp>
#include "externalprocess.h"

BOOST_AUTO_TEST_CASE(TestExternal) {
    mkdir("cache", 0755);
    ExternalProcess cde;
    BOOST_REQUIRE_EQUAL(cde.open("./cde", "-Ccache"), true);

    // ack
    cde.send("A tests/code/simple.cpp\n");
    BOOST_CHECK_EQUAL(cde.recv(), "(setq-local cde--project \"tests/code\")\n");
    BOOST_CHECK_EQUAL(cde.recv(), "(cde--hideif \"tests/code/simple.cpp\" '((8 9)))\n");
    BOOST_CHECK_EQUAL(cde.recv(),"(cde--error-rep nil nil nil)\n");
    // complete
    // quit

    // restore index
    // ref
    // def

    // quit
    cde.send("Q\n");
    BOOST_CHECK_EQUAL(cde.recv(),
                      "(setq cde--process nil)(save-buffers-kill-terminal)\n");

}
