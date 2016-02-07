var webpage = require('webpage');

var page = webpage.create();

var previousTestHasFailed = false;

page.onConsoleMessage = function(msg) {
  if (msg === "TESTS_ARE_FINE") {
    if (previousTestHasFailed) {
      phantom.exit(1);
    } else {
      phantom.exit(0);
    }
  } else if (msg === "SOME_TEST_HAS_FAILED") {
    previousTestHasFailed = true;
  } else {
    console.log(msg);
    if (previousTestHasFailed) {
      phantom.exit(1);
    }
  }
};

page.open('http://localhost:7000/test/index.html');
