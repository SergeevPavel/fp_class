-- 1. fib n вовзращает n-ое число Фибоначчи.
--    Функция должна работать за линейное вермя и определена для всех целых n.
--    Для отрицательных n значение определяется по формуле fib n = fib (n + 2) - fib (n + 1).
--    (1 балл)
fib :: Integer -> Integer
fib n = fst $ fib' n
    where
        fib' :: Integer -> (Integer, Integer)
        fib' 0 = (0, 1)
        fib' n = if n > 0 then (s, f + s) else (s' - f', f')
                    where
                        (f, s) = fib' (n - 1)
                        (f', s') = fib' (n + 1)

-- 2a. Написать функцию, возвращающую количество цифр числа.
--     Для целочисленного деления можете использовать функции div и mod.
--    (0.5 балла)
numberOfDigits :: Integer -> Integer
numberOfDigits n | n < 0 = numberOfDigits $ negate n
                 | n < 10 = 1
                 | otherwise = 1 + (numberOfDigits $ div n 10)  

-- 2b. Написать функцию, возвращающую сумму цифр числа.
--    (0.5 балла)
sumOfDigits :: Integer -> Integer
sumOfDigits n | n < 0 = sumOfDigits $ negate n
              | n < 10 = n
              | otherwise = m + (sumOfDigits d)
                    where
                        (d, m) = divMod n 10

-- 3. gcd' возвращает НОД.
--    (1 балл)
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- 4. minp p возвращает минимальное по модулю число x такое, что p x == True. Если такого x не существует, minp не завершается.
--    (1 балл)
minp :: (Integer -> Bool) -> Integer
minp p = test (0, 0)
        where
            test :: (Integer, Integer) -> Integer
            test (a, b) | p a = a
                        | p b = b
                        | otherwise = test (a - 1, b + 1)

-- 5. integral f a b возвращает значение определенного интеграла функции f на отрезке [a,b].
--    Для реализации можете использовать метод трапеций.
--    (2 балла)
integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = helper a (f a) 0
    where
        helper :: Double -> Double -> Double -> Double
        helper x fx sum | x >= b = sum
                        | otherwise = helper y fy (sum + ds)
            where
                step = 0.001
                dx = min step (b - x)
                y = x + dx
                fy = f y
                ds = (fx + fy) / 2 * dx


-- 6. Реализуйте оператор примитивной рекурсии rec, используя функцию (-), укажите тип rec.
--    (1 балл)
rec :: a -> (Integer -> a -> a) -> Integer -> a
rec z s 0 = z
rec z s n = s n (rec z s (pred n))
 

-- 7. Реализуйте факторил при помощи rec.
--    (1 балл)
facRec :: Integer -> Integer
facRec n = rec 1 (*) n

-- 8. Реализуйте факториал при помощи fix.
--    (1 балл)
facFix :: Integer -> Integer
facFix =  fix (\ f n -> if (n == 0) then 1 else (n * f(pred n)))
  where fix f = f (fix f)
