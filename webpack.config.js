const path = require('path');

module.exports = {
  mode: 'development',
  devtool: 'source-map',
  entry: './dist/index.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist')
  },
  devServer: {
    contentBase: path.join(__dirname, 'dist'),
    compress: true,
    watchContentBase: true,
    port: 9000
  },
  module: {
      rules: [
          {
              test: /\.purs$/,
              use: [
                  {
                      loader: 'purs-loader',
                      options: {
                          src: [
                              'bower_components/purescript-*/src/**/*.purs',
                              'src/**/*.purs'
                          ],
                          bundle: false,
                          psc: 'psa',
                          pscIde: false
                      }
                  }
              ]
          },
      ]
  },
};
