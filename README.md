# haskell-uon

## Что это?

Библиотека для доступа к [U-ON API](https://api.u-on.ru/doc).
Обязательные/опциональные типы возвращаемых полей подобраны экспериментальным путём.

## Как пользоваться?

### Stack:

Добавить зависимость в stack.yaml, с нужным хешем коммита.
```yaml
extra-deps:
- git: https://github.com/mejgun/haskell-uon.git
  commit: a4bd75bd7621a2b0873688dbc24cfaf26c1f14ba
```

### Пример

```haskell
import UON qualified (newKey, pause)
import UON.Request.Search qualified as Request

main = do
  [key] <- UON.newKey . T.pack <$> getArgs
  UON.pause
  r <- Request.search key [ Request.Page 1 ]
      case r of
        Right x -> print x
        Left e -> error $ T.unpack e
```