const path = require('path');

module.exports = {
  devtool: 'eval',
  entry: './webpack.entry.js',
  output: {
    path: path.join(__dirname, 'dist'),
    pathinfo: true,
    filename: 'index.js'
  }
};