const path = require("path");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const { DefinePlugin } = require('webpack')

const SOURCE_DIR = path.join(__dirname, 'src')

const mode = process.env.NODE_ENV === 'production'
  ? 'production'
  : 'development'

// Copy the specified environment variables into an object we can pass to
// webpack's DefinePlugin
const copyArgs = (args) =>
  args.reduce(
    (acc, key) => ({
      // Create an object with the specified key
      ...acc,
      [`process.env.${key}`]: JSON.stringify(process.env[key]),
    }),
    {}
  )

module.exports = {
  mode,

  watch: mode === 'development',
  
  entry: {
    app: [
      './src/index.js'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js',
  },

  module: {
    rules: [
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  'elm-webpack-loader',
      },
    ],

    noParse: /\.elm$/,
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: path.join(SOURCE_DIR, 'index.html'),
      minify: {
        collapseWhitespace: true,
        removeComments: true
      }
      // favicon: path.resolve('./static/favicon.png')
    }),
    new DefinePlugin({
      ...copyArgs([
        'API_ENDPOINT',
      ]),
    })
  ],

  devServer: {
    inline: true,
    stats: { colors: true },
  },
};
