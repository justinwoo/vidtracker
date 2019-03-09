// this is the entry point
// we import the build output of module Main and run main
function main() {
  require("../output/FrontEnd").main();
}

// why this code? because we use parcel
// read through https://parceljs.org/
if (module.hot) {
  module.hot.dispose(function() {
    window.location = window.location;
  });
}

main();
