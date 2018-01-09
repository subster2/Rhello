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

Нахождение потенциала:
```
g_potentials <- function(iris, Core, h) {
  eps <- 0.2 
  xl <- iris[, 3:5]                  
  l <- dim(xl)[1]
  potent <- c()
  for(i in 1:l) {
    potent[i] <- 0
  }
  while(LOO_gamma(iris, potent, Core, h) > eps) {
    random <- sample(i, 1, replace = T) # random number in 1:l
    random_point <- c(xl[random, 1], xl[random, 2]) # choose random x_i
    if(pf(iris, random_point, potent, Core, h) != xl[random, 3]) { #if algorithm is mistaken on x_i, then potential i object ++
      potent[random] <- potent[random] + 1
    }
  }
  return(potent)
}
```

Классификации объектов находящихся в диапазоне расположения объектов **x(i)** выборки **Xl**, для прямоугольного ядра ядра при **h=0.4** (черные - точки у которых ненулевой потенциал):


![alt text](https://github.com/subster2/Rhello/blob/master/PF/PFh%3D0.4CoreTriang.png)

### Преимущества:
I. "Богатый" набор из **2l** параметров.

### Недостатки:

I. Классификация зависит от удачного подбора окна **h**.

II.  Нужно хранить всю выборку.

III.  Количество итераций алгоритма неизвестно, т.к. объекты **x_i** выбираются случайным образом.

IV.  Из-за подсчёта ошибок сложность каждой итерации составляет **O(l^2)**, а самих итераций неизвестно.

V.  Если в окно не попало ни одной точки , то алгоритм не будет работать.

## Сравнительная таблица метрических алгоритмов

 <table border = 2 width = "60%">
   <tr>
      <td width = "30%" align = center><b>Алгоритм</b></td>
      <td width = "15%" align = center><b>kNN</i></td>
      <td width = "15%" align = center><b>KwNN</i></td>
      <td width = "15%" align = center><b>Парзеновское окно</i></td>
   </tr>
   
   <tr>
      <td width = "30%" align = center><b>Оптимальный параметр</b></td>
      <td width = "15%" align = center><b>k_opt = 6</td>
      <td width = "15%" align = center><b>q_opt = 0.6</td>
      <td width = "15%" align = center><b>h_opt = 0.4</td>
   </tr>
      
   <tr>
      <td width = "30%" align = center><b>LOO optim</b></td>
      <td width = "15%" align = center><b>0.0(3)</td>
      <td width = "15%" align = center><b>0.04</td>
      <td width = "15%" align = center><b>0.04</td>
   </tr> 
   
   <tr>
      <td width = "30%" align = center><b>number of mistakes</b></td>
      <td width = "15%" align = center><b>5</td>
      <td width = "15%" align = center><b>6</td>
      <td width = "15%" align = center><b>6</td>
   </tr> 
 </table>
 
 ## Понятие отступа и алгоритм STOLP
 
  *Отступ* показывает степень типичности объекта. Отступ отрицателен тогда и только тогда, когда алгоритм допускает ошибку на данном объекте. В зависимости от значений отступа обучающие объекты условно делятся на пять типов, в порядке убывания отступа: эталонные,неинформативные,пограничные, ошибочные, шумовые:
  
• Эталонные объекты имеют большой положительный отступ, плотно окружены объектами своего класса и являются наиболее типичными его представителями.

• Неинформативные объекты также имеют положительный отступ. Они не добавляют к эталонам никакой новой информации. Наличие
неинформативных объектов характерно для выборок избыточно большого объема.

• Пограничные объекты имеют отступ, близкий к нулю. Классификация таких объектов неустойчива в том смысле, что малые изменения
метрики или состава обучающей выборки могут изменять их классификацию.

• Ошибочные объекты имеют отрицательные отступы и классифицируются неверно. Возможной причиной может быть неадекватность алгоритмической модели, в частности, неудачная конструкция метрики ρ.

• Шумовые объекты или выбросы — это небольшое число объектов с большими отрицательными отступами. Они плотно окружены объектами чужих классов и классифицируются неверно. Они могут возникать из-за грубых ошибок или пропусков в исходных данных, а также по причине отсутствия важной информации, которая позволила бы отнести эти объекты к правильному классу.

Реализация отступа на примере выборки ирисов:

![alt text](https://github.com/subster2/Rhello/blob/master/Margin/Margin.png)

Красный цвет - выбросы

Желтый цвет - пограничные объекты

Светло-зеленый - неинформативные объекты

Зеленый - эталонные объекты

Реализация кода : [margin](https://github.com/subster2/Rhello/blob/master/Margin/margin.R)

### Алгоритм STOLP

  Суть алгоритма состоит в том чтобы на первом шаге найти все выбросы и изъять их из выборки, вторым шагом найти эталонный объект с наибольшим положительным отступом для каждого класса и создать из них выборку(**Ω**). В цикле пока выборка с эталонными объектами  не равна обучающей выборке (**Ω!= X**): 
  
  I. Выделить множество объектов, на которых алгоритм a(u, Ω) ошибается: **E = {xi ∈ X\ Ω | M(xi, Ω) < 0}.**
  
  II. Если **|E| < L**, где **L** - допустимая доля ошибок то: выход из цикла
  
  III. Присоединить к Ω объект с наименьшим отступом: **xi = argmin(M(x, Ω)), Ω = Ω ∪ {xi}**. Вернуться к пункту I.
  
  Полученная картинка по данному алгоритму для выборки ирисов
  
  ![alt text](https://github.com/subster2/Rhello/blob/master/Margin/STOLP.png)
  
  Пустые круги розового цвета - удаленные выбросы
  
  Пустые круги синего, зеленного, красного цвета - неинформативные объекты
  
  Заполненные круги - эталонные объекты.

  




 
 
 
 
 # Байесовские алгоритмы классификации
 
  Байесовский подход является классическим в теории распознавания образов и лежит в основе многих методов. Он опирается на теорему о том, что если плотности распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде.

 ## Нормальный дискриминантный анализ
 
  Нормальный дискриминантный анализ — это специальный случай байесовской классификации, когда предполагается, что плотности всех классов **py(x), y ∈ Y** , являются многомерными нормальными. В этом случае задача оценивания параметров распределения по выборке решается аналитически.
  Вероятностное распределение с плотностью
  
  ![alt text](https://github.com/subster2/Rhello/blob/master/NDA/NDAform.png).
   
  называется **n**-мерным многомерным нормальным (гауссовским) распределением с математическим ожиданием (центром) **µ ∈ Rn** и ковариационной матрицей **Σ ∈ Rn×n**. Предполагается, что матрица **Σ** симметричная, невырожденная, положительно определенная.
  
  1) Если признаки некоррелированы, **Σ = diag(σ1, . . . , σ2)**, то линии уровня плотности распределения имеют форму эллипсоидов с центром **µ** и осями, параллельными осям координат.
  
  ![alt text](https://github.com/subster2/Rhello/blob/master/NDA/LineGaus2.png).
  
  2) Если признаки имеют одинаковые дисперсии, **Σ = σ^2In**, то эллипсоиды являются сферами.
   
   ![alt text](https://github.com/subster2/Rhello/blob/master/NDA/LineGaus1.png).
   
   3) Если признаки коррелированы, то матрица **Σ** не диагональна и линии уровня имеют форму эллипсоидов, оси которых повернуты (направлены вдоль собственных векторов матрицы **Σ**) относительно исходной системы координат.

   ![alt text](https://github.com/subster2/Rhello/blob/master/NDA/LineGaus4.png).

  Программная реализация : 
  ```
library(mvtnorm)
x.points <- seq(-3,3,length.out=100)
y.points <- x.points
z <- matrix(0,nrow=100,ncol=100)
mu <- c(0,0)
sigma <- matrix(c(2,1,1,1),nrow=2)
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),
                      mean=mu,sigma=sigma)
  }
}
contour(x.points,y.points,z)
  ```
 
 
 
 ## «Наивный» байесовский классификатор
 
Теорема Байеса позволяет переставить местами причину и следствие. Зная с какой вероятностью причина приводит к некоему событию, эта теорема позволяет расчитать вероятность того что именно эта причина привела к наблюдаемому событию.


### Теорема
Для событий **A и B**, при условии, что **P(B) ≠ 0**,

![alt text](https://wikimedia.org/api/rest_v1/media/math/render/svg/39c91420515c75cb65cd09a4dad706e1197a29ee).


Цель классификации состоит в том чтобы понять к какому классу принадлежит элемент, поэтому нам нужна не сама вероятность, а наиболее вероятный класс. Байесовский классификатор использует оценку апостериорного максимума (Maximum a posteriori estimation) для определения наиболее вероятного класса. Грубо говоря, это класс с максимальной вероятностью. То есть нам надо рассчитать вероятность для всех классов и выбрать тот класс, который обладает максимальной вероятностью.

Формула рассчета:

![alt text](https://github.com/subster2/Rhello/blob/master/BAYES/BayesForm.jpg)

Сам код : [bay](https://github.com/subster2/Rhello/blob/master/BAYES/bayes.r)

Пример:


![alt text](https://github.com/subster2/Rhello/blob/master/BAYES/bay1.png)

### Преимущества:

I. Простота реализации.

II. Низкие вычислительные затраты при обучении и классификации.

III. Когда признаки близки к независимости , классификатор близок к оптимальному.

### Недостатки

I.Низкое качество классификации.


