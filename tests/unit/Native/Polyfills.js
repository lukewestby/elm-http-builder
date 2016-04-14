var Elm = Elm || {};
Elm.Native = Elm.Native || {};
Elm.Native.Polyfills = {};
Elm.Native.Polyfills.make = function (elm) {
  global.FormData = function () { this._data = []; };
  Object.defineProperty(global.FormData.prototype, 'append', {
    value: function () {
      this._data.push(Array.prototype.slice.call(arguments));
    },
    enumerable: false,
  });

  elm.Native = elm.Native || {};
  elm.Native.Polyfills = {};
  elm.Native.Polyfills.values = {
    enable: true,
  };
  return elm.Native.Polyfills.values;
};
