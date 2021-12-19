## Обща информация
### Краен срок: 19.12.2021
### Максимум точки без бонуси: ?

## Структура

## Предварителни стъпки
Инсталирайте `stack`:
  * Ако сте си инсталирали `ghc` посредством [`Haskell Platform`](https://www.haskell.org/platform/),
    то вече имате и `stack`.
  * В противен случай - [инструкции за инсталиране](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
  * Ако имате `ghcup`, можете и чрез него също да инсталирате `stack`.

## Пускане на домашното и тестовете му
**N.B. Първото пускане ще отнеме сравнително голямо количество време,
тъй като `stack` тегли специфична версия на `ghc` и комплира специфични пакети за нея, които са проверени че работят заедно**

**N.B. За домашното съм пуснал `-Werror`, което означава че всички пуснати предупреждения са
_комплационни грешки_. Очаква се да предадете домашно което се компилира под тези условия.**

Ако желаете да изключите за малко това поведение може да закоментирате (с `#`) `-Werror` реда от `package.yaml`,
но не забравяйте да го пуснете отново след това.

Извиквания на `stack`:
* `stack build` - компилира source файловете ви
* `stack ghci` - пуска `ghci` в което са заредени source файловете ви
* `stack test` - пуска тестовете към домашното

Флагът `--file-watch` на `stack` е много удобен - когато го подадете към
`stack build/test` автоматично прекомпилира (и съответно пуска тестовете),
при промяна на някой source файл.

Примери:
* `stack test --file-watch`

## Препоръчителна чистота на кода

Можете (и препоръчвам) да си инсталирате `hlint`:

`stack install hlint`

След това можете да пуснете `hlint` върху вашите файлове,
за да ви препоръча начини да подобрите кода си.

Това е полезно за вас защото
* се учите да пишете по-"идиоматичен" Haskell
* не трябва да ме чакате за feedback, за сравнително голяма част от коментарите, които бих сложил