# Changelog

`pagure-hs` uses [PVP Versioning](https://pvp.haskell.org).

## 0.1.1 (2022-02-17)
- add queryPagure' which errors when JSON response has "error" field
  and use it for queryPagureCount and queryPagurePaged
  This requires http-query-0.1.2 or later to avoid early exceptions
  and get meanful error messages instead

## 0.1.0 (2021-12-27)
* Initial Hackage release
