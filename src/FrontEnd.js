exports.addWindowKeyListener = function(effect) {
  return function() {
    window.addEventListener("keypress", function(e) {
      effect(e.key)();
    });
  };
};
