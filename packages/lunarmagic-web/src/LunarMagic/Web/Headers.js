/**
 * @param {HeadersInit} headersInit
 */
export const newFn = (headersInit) => {
  return new Headers(headersInit);
}

/**
 * 
 * @param {string} key 
 * @param {string} value 
 * @param {Headers} headers 
 */
export const appendFn = (key, value, headers) => {
  headers.append(key, value);
}

/**
 * 
 * @param {string} key 
 * @param {Headers} headers 
 */
export const deleteFn = (key, headers) => {
  headers.delete(key);
}

/**
 * 
 * @param {string} key 
 * @param {Headers} headers 
 * @returns 
 */
export const getFn = (key, headers) => {
  return headers.get(key);
}

/**
 * 
 * @param {string} key 
 * @param {Headers} headers 
 */
export const hasFn = (key, headers) => {
  return headers.has(key);
}

/**
 * 
 * @param {string} key 
 * @param {string} value 
 * @param {Headers} headers 
 * @returns 
 */
export const setFn = (key, value, headers) => {
  return headers.set(key, value);
}

/**
 * 
 * @param {Headers} headers 
 */
export const getSetCookieFn = (headers) => {
  return headers.getSetCookie();
}