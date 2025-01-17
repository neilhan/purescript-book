"use strict";

exports.setItem = key => value => () =>
  window.localStorage.setItem(key, value);

exports.getItem = key => () =>
  window.localStorage.getItem(key);

exports.removeItem = key => () =>
  window.localStorage.removeItem(key);

exports.confirm = msg => () => window.confirm(msg)