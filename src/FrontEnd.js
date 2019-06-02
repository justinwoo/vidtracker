var ReactDOM = require('react-dom');

exports.renderJSX = function (jsx) {
  return function () {
    ReactDOM.render(jsx, document.getElementById('app'))
  }
}

exports.addWindowKeyListener = function(effect) {
  return function() {
    window.addEventListener("keypress", function(e) {
      effect(e.key)();
    });
  };
};

exports.refreshPage = function() {
  window.location = window.location;
};

exports.scrollIntoView = function(selector) {
  return function() {
    var e = document.querySelector('div[title="' + selector + '"]');
    if (e) {
      if (e.scrollIntoViewIfNeeded) {
        e.scrollIntoViewIfNeeded();
      } else {
        e.scrollIntoView();
      }
    }
  };
};

exports.scrollToTop = function() {
  window.scroll(0, 0);
};
