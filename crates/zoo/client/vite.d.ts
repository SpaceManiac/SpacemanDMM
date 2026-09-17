declare module "*.css" {}
declare module "*?url" {
  const url: string;
  export default url;
}
