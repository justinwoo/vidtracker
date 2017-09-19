exports.jsonBodyParser = require('body-parser').json();

exports._getBody = function (req) {
  return req.body;
};
