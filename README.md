# KNN (K-nearest neighbors)
**Задача классификации в машинном обучении** — это задача отнесения объекта к одному из заранее определенных классов на основании его формализованных признаков. Каждый из объектов в этой задаче представляется в виде вектора в N-мерном пространстве, каждое измерение в котором представляет собой описание одного из признаков объекта.Для обучения классификатора необходимо иметь набор объектов, для которых заранее определены классы. Это множество называется обучающей выборкой, её разметка производится вручную, с привлечением специалистов в исследуемой области. 

**Алгоритм**

Для классификации каждого из объектов тестовой выборки необходимо последовательно выполнить следующие операции:

 - 	Вычислить расстояние до каждого из объектов обучающей выборки
 -  Отобрать k объектов обучающей выборки, расстояние до которых минимально
 -	Класс классифицируемого объекта — это класс, наиболее часто встречающийся среди k ближайших соседей
  
**Метод ближайших соседей** — простейший метрический классификатор, основанный на оценивании сходства объектов. Классифицируемый объект относится к тому классу, которому принадлежат ближайшие к нему объекты обучающей выборки.

Для любого объекта **u∈Х** расположим элементы обучающей выборки **x1,x2,...,x** по мере возрастания расстояния до **u** Метрический алгоритм классификации с обучающей выборкой **X** относит объект u к тому классу **y∈Y** , для которого суммарный вес ближайших обучающих объектов максимален:


 
 где весовая функция **ω(i, u)** оценивает степень важности i-го соседа для классификации объекта **u**. Эта функция неотрицательна и не возрастает по i.
Если по-разному задавать весовую функцию, можно получать различные варианты метода ближайших соседей.
- **ω(i, u) = [i = 1]** — простейший метод ближайшего соседа;
- **ω(i, u) = [i =< k]** — метод k ближайших соседей;
- **ω(i, u) = [i =< k]qi** — метод k экспоненциально взвешенных ближайших соседей, где предполагается q < 1;
Рассмотрим KNN для i =< k

**ДОСТОИНСТВА МЕТОДА kNN** 
- Программная реализация алгоритма относительно проста. 
- Возможность модификации алгоритма. 
- Алгоритм устойчив к аномальным выбросам. 
- Возможность интерпретации результатов работы алгоритма.

**НЕДОСТАТКИ МЕТОДА kNN**  
- Набор данных, используемый для алгоритма, должен быть репрезентативным. 
- Необходимость хранить обучающую выборку целиком. 
- В простейших случаях метрические алгоритмы имеют крайне бедный набор параметров, что исключает возможность настройки алгоритма по данным. 
- Затраты в производительности велики, поскольку нам необходимо вычислить расстояния между каждым экземпляром и всеми пробными экземплярами.

