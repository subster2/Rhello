# Метрические алгоритмы классификации

**Метрический классификатор (similarity-based classifier)**  - алгоритм классификации, основанный на вычислении оценок сходства между объектами.

Иными словами метрический классификатор основан на **гипотеза компактности** (Схожим объектам соответствуют схожие ответы).
Схожесть объектов мы можем определить введя в **функцию расстояния** ![](http://www.machinelearning.ru/mimetex/?\rho(x,x%27)) в пространстве объектов **Х**.

Пример сложной задачи для классификации:  
![alt text](https://github.com/subster2/Rhello/blob/master/KNN-simple/%D0%91%D0%B5%D0%B7%D1%8B%D0%BC%D1%8F%D0%BD%D0%BD%D1%8B%D0%B9.png)


## Алгоритм k ближайших соседей (kNN)

Для любого объекта **u∈Х** расположим элементы обучающей выборки **x1,x2,...,xl** по мере возрастания расстояния до **u** 
Метрический алгоритм классификации с обучающей выборкой **X** относит объект **u** к тому классу **y∈Y** , для которого суммарный вес
ближайших обучающих объектов максимален:

![](http://www.machinelearning.ru/mimetex/?a(u)%20=%20\mathrm{arg}\max_{y\in%20Y}%20\sum_{i=1}^m%20\bigl[%20x_{i;%20u}=y%20\bigr]%20w(i,u),)
где весовая функция **ω(i, u)** оценивает степень важности **i**-го соседа для классификации объекта **u**. Эта функция неотрицательна и не возрастает по **i**.

Если по-разному задавать весовую функцию, можно получать различные варианты метода ближайших соседей.

* **ω(i, u) = [i = 1]** — простейший метод ближайшего соседа;
* **ω(i, u) = [i =< k]** — метод **k** ближайших соседей;
* **ω(i, u) = [i =< k]qi** — метод **k** экспоненциально взвешенных ближайших соседей, где предполагается **q < 1**;

Рассматривать метод ближайшего соседа (1NN) смысла нет. Его недостатки и так ясны (Неустойчивость к погрешностям, зависимость от "удачности" выбора метрики, низкое качество классификации) 

Рассмотрим **KNN** для **i =< k**
```
KNN <- function(yl , point_to_classify, k , metric = euclideanDistance){
  distances <- c()
  for(i in 1:nrow(yl)){
    distances[i] <- metric(yl[i , 1:length(yl) - 1] , point_to_classify) # рассчитываем расстояние каждой точки классов до u
  }
  yl <- cbind(yl , distances)
  ordered_dist_array <- yl[order(distances),]
  k_arr <- ordered_dist_array[1:k , 3]
  class_iris <- table(k_arr) 
  return(names(which.max(class_iris))) # возвращаем название класса к которому принадлежит точка
}
```
для ознакомление с полным кодом : [KNN](KNN-simple/KNN.R)

Пример выборки IrisFishers
![alt text](https://github.com/subster2/Rhello/blob/master/KNN-simple/RKnn.png)

А также наглядно преймущество **KNN**(в данном случае **k=2**) над **1NN**
![alt text](https://github.com/subster2/Rhello/blob/master/KNN-simple/newNN.png)

### Преимущества:
I. Простота реализации.

II. При **k** (оптимальном), алгоритм "неплохо" классифицирует.

### Недостатки:
I. Нужно хранить всю выборку.

II. При **k = 1** неустойчивость к погрешностям , выброс(погрешность) классифицируется неверно окружающие его объекты.

III. При **k = l** алгоритм вырождается в константу.

IV. Малый набор параметров.

V. Точки с одинаковым расстоянием вызывают неопределенность 

## LOO (Leave one out) для определения лучшего К

LOO или критерий скользящего контроля:
1. Исключаем объекты **x(i)** из выборки **Xl** по одному, получаем новую выборку без объекта **x(i)** назовем её **Xl_1**.
2. Запускаем алгоритм объекта **u**, который классифицируем на выборке **Xl_1**.
3. Создадим переменную **Q** (накопитель ошибок)  когда алгоритм ошибается увеличиваем ее на 1.
4. Когда все объекты **x(i)** будут перебраны, вычислить **LOO = Q / l**, где **l** - количество объектов выборки.

Рассмотрим работу LOO для KNN: лучший результат классификации будет достигаться при **K=7**, а значение критерия **Q = 0.0333(3)**
Сам код: [LOO](https://github.com/subster2/Rhello/blob/master/KNN%20withh%20LOO/KNNwithLOO.R)

![alt text](https://github.com/subster2/Rhello/blob/master/KNN%20withh%20LOO/KNNloo.png)


## Алгоритм k взвешенных ближайших соседей (wkNN)

Для оценки близости используется функция:

**ω(i, u) = [i =< k]qi** — метод **k** экспоненциально взвешенных ближайших соседей, где предполагается **q < 1**

Реализация здесь : [wKNN](https://github.com/subster2/Rhello/blob/master/KNN-weight/KNN-w.R)

Лучший показатель **LOO = 0.04**, а параметр **q = 0.56**

Пример для для оптимального q и не оптимальных:

![alt text](https://github.com/subster2/Rhello/blob/master/KNN-weight/KwNN3.png)


Алгоритм **k** взвешенных ближайших соседей учитывает порядок объектов при классификации и выдаёт более точный результат чем KNN. Объекты находящиеся ближе к классифицируемому объекту будут больше влиять на него(из-за учёта порядка объектов).


## Метод парзеновского окна

В данном алгоритме весовая функция **w_i** определяется как функция от расстояния между классифицируемым объектом u и его соседями **x(u_i), i = 1, ..., l,** а не от ранга соседа **i**.

![](https://latex.codecogs.com/gif.latex?W%28i%2C%20u%29%20%3D%20K%5Cleft%20%28%20%5Cfrac%7B%5Crho%20%28u%2C%20x_%7Bu%7D%5E%7Bi%7D%29%7D%7Bh%7D%20%5Cright%20%29) , где **K(y)** -- функция ядра , а **h** -- ширина окна

Для того чтобы понять метод можно привести аналогию: в темной комнате на стене множество точек разных цветов. Для того чтобы определить какого цвета точка нужно посветить на неё так, чтобы центр луча фонаря был на ней. И все точки которые попадут в радиус луча будут "голосовать" за свой цвет. Чьих "голосов" будет больше, того цвета опеределяемая точка.

Пример прямоугольного ядра и LOO для него  


![](https://latex.codecogs.com/gif.latex?K%28z%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%5B%7Cz%7C%20%3C%3D%201%5D)
![alt text](https://github.com/subster2/Rhello/blob/master/Parz/ParzenRect.png)

Пример гауссового ядра и LOO для него 


![](https://latex.codecogs.com/gif.latex?K%28z%29%20%3D%20%5Cfrac%7B15%7D%7B16%7D*%281%20-%20z%5E%7B2%7D%29%5B%7Cz%7C%20%3C%3D%201%5D)
![alt text](https://github.com/subster2/Rhello/blob/master/Parz/Parzgauss.png)

LOO показывает оптимальный параметр **h**, который определяет ширину самого окна.

### Преимущества:
I. Простота реализации.

II. Не требуется сортировка расстояний => скорость работы быстрее  .

III. Точки попадающие в окно, расстояние между которыми одинаково будут влиять на результат(в отличие от алгоритма  KwNN).

IV.Окно с переменной шириной решает проблему разрешимости задач, в которых обучающие выборки распределены неравномерно.

### Недостатки:
I. Неустойчивость при слишком узком и широком окне.

II. Диапазон параметра h, нужно подбирать (через LOO).

III. "Скудный" набор параметров.

IV. Если в окно не попало ни одной точки , то алгоритм не будет работать.

V. Проблема классификации при равных весах классов.


## Метод потенциальных функций

Кардинальное отличие от метода парзеновского окна в том, что окно строиться не над классифицируемом объекте, а над объектами выборки для определения их потенциала(важности для классификации). В простом случае посчитаем что ширина окна нам известна, тогда потребуется найти только потенциалы. Для оценки близости классифицируемого объекта  к классу используем функцию:

![](https://latex.codecogs.com/gif.latex?W%28i%2C%20u%29%20%3D%20%5Cgamma_%7Bi%7D*K%5Cleft%20%28%20%5Cfrac%7B%5Crho%20%28u%2C%20x_%7Bu%7D%5E%7Bi%7D%29%7D%7Bh_i%7D%20%5Cright%20%29%2C%20%5Cgamma_%7Bi%7D%20%5Cgeqslant%200%2C%20h_i%20%3E%200). 

где ![](https://latex.codecogs.com/gif.latex?%5Cgamma_%7Bi%7D). - потенциал


Карта классификации всех объектов , находящихся в диапазоне расположения объектов **x(i)** выборки **Xl**, для прямоугольного ядра ядра (черные - точки у которых ненулевой потенциал):





