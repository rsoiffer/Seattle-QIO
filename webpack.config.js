// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

const path = require("path");

module.exports = {
  mode: "development",
  entry: {
    app: [ "./src/App.fsproj", "./src/style.sass" ]
  },
  output: {
    path: path.join(__dirname, "./public"),
    filename: "bundle.js",
  },
  devServer: {
    publicPath: "/",
    contentBase: "./public",
    port: 8080,
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: "fable-loader"
      },
      {
        test: /\.sass$/,
        use: [ "style-loader", "css-loader", "sass-loader" ]
      },
      {
        test: /\.(ttf|woff|woff2)$/,
        use: [ "file-loader" ]
      }
    ]
  }
}
