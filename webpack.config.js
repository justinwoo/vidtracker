const path = require('path');
const webpack = require('webpack');

module.exports = {
  devtool: 'eval',
  entry: './dist/frontend-prebundle.js',
  externals: {
    echarts: 'echarts'
  },
  output: {
    path: path.join(__dirname, 'dist'),
    pathinfo: true,
    filename: 'index.js'
  }
};