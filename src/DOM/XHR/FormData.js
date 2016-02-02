// module DOM.XHR.FormData

exports.empty = function() { return new FormData(); };

exports.insert = function(key) {
  return function(value) {
    return function(formData) {
      return function() {
        formData.append(key, value);
        return formData;
      }
    }
  }
};
