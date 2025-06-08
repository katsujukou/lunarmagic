/**
 * 
 * @param {string} url 
 * @param {string} base 
 * @returns 
 */
export const newFn = (url, base) => {
  return new URL(url, base)
}

export const canParseFn = (url) => {
  return URL.canParse(url);
}

export const canParseWithBaseFn = (url, base) => {
  return URL.canParse(url, base);
}

export const parseFn = (Nothing, Just, urlString) => {
  return parseWithBaseFn(Nothing, Just, urlString);
}

export const parseWithBaseFn = (Nothing, Just, urlString, base) => {
  const url = URL.parse(urlString, base);
  if (url) {
    return Just(url);
  } 
  return Nothing;
}