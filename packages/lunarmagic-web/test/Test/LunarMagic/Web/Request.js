export const newRequest = (method, url, body) => {
  return new Request (url, { method, body });
}
