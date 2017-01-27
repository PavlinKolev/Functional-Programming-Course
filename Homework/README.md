##Домашно №1 за всички групи - условия


Зад.1. (2т.) Нека e дадено естественото число n. Ако n е четно, преминете към n/2, а в противен случай към 3n+1, и продължете от "новото" число. Изпробвано е, че за всяко число n до ~260 тази последователност със сигурност ще достигне 1 след краен брой стъпки. Напишете функция hailstone, която по дадено n генерира тази последователност от числа до момента, в който n достигне 1:
hailstone 1 -> [1]
hailstone 4 -> [4,2,1]
hailstone 10 -> [10,5,16,8,4,2,1]

Зад.2. (2т.) Намерете броя на двуцифрените нечетни съставни числа, които не могат да се представят като сбор на някое просто число и два пъти по някой точен квадрат (напр. 39 не е такова число, т.к. може да се представи като 7 + 2*42).
Упътване: list comprehension би ви улеснил живота.

Зад.3. (2т.) Напишете функция divisors, която по дадено естествено число генерира списък от всичките му прости делители и тяхната кратност (няма значение в какъв ред). Помнете, че 1 не е просто число, а и не се брои за делител:
divisors 120 -> [(2,3),(3,1),(5,1)]  -- 120 = 23.31.51
divisors 127 -> [(127,1)]            -- няма какво друго, след като 127 е просто

Зад.4. (2т.) Напишете функция intercalate', която приема стринг x и списък от стрингове xs и връща стринга, получен от вмъкването на x между всеки два съседни стринга в xs:
intercalate' ", ha, " ["this", "sounds", "funny"] -> "this, ha, sounds, ha, funny"

Заб.: В модула Data.List има точно такава функция, използването ѝ (както и на intersperse) е очевидно забранено. Обърнете внимание, че след "funny" няма друго ", ha, ".

Зад.5. (2т.) Нека е даден списък от числа с дължина 2n за някое естествено n. Напишете функция fenwick, която построява пълно балансирано двоично дърво с височина n такова, че:
- стойностите в листата са елементите от дадения списък, подредени в същия ред "отляво-надясно" в дървото
- стойността във всеки вътрешен възел е сумата от стойностите на двата му директни наследника. (тоест в корена ще е сумата от всички числа в списъка).
fenwick [1,3,5,-1,2,0,-4,3] ->
          9
    8           1
 4     4     2    -1 
1 3   5 -1  2 0  -4 3
Бонус (1т.): допълнете функцията така, че да работи за списъци с всякаква дължина.

Зад.6. (2т.) За целите на тази задача ще казваме, че две думи могат да бъдат "залепени", ако последната буква на първата съвпада с първата буква на втората дума:
"string" + "google" = "stringoogle".
Напишете функция longestWord, която по списък от думи намира най-дългата дума, която може да се получи от "залепване" на една или повече думи от списъка (без повторения):
longestWord ["strings", "safe", "get", "setting", "elon"] -> "stringsettinget"
-- "strings" + "setting" + "get" е по-дълга от "strings" + "safe" + "elon" или само "safe" + "elon"