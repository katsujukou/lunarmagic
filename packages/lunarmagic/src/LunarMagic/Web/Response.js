/**
 * 
 * @param {Response} resp 
 * @returns 
 */
export const clone_ = (resp) => {
  return resp.clone();
}

export const new_ = (bodyInit, responseInit) => {
  return new Response(bodyInit, responseInit);
}