var ReactDOM = require("react-dom");

exports.renderJSX = function(jsx) {
  return function() {
    ReactDOM.render(jsx, document.getElementById("app"));
  };
};
