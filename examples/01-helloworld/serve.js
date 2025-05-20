import { fetch } from "../../output-es/Example.HelloWorld/index.js";

Bun.serve({
  port: 3000,
  async fetch (req) {
    const resp = await fetch(req);    
    return resp;
  }
})