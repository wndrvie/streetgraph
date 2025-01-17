# streetgraph
[![Build Status](https://travis-ci.org/wndrvie/streetgraph.svg?branch=master)](https://travis-ci.org/wndrvie/streetgraph)
[![Build status](https://ci.appveyor.com/api/projects/status/irg2rlaxtq6slrhc/branch/master?svg=true)](https://ci.appveyor.com/project/wndrvie/streetgraph/branch/master)

Программа, строящая граф дорог города Архангельск.

## Структура
Основной исходный код, снабженный комментариями, можно найти в папке
src/streetgraph. Там присутствуют следующие модули:

- **core**: main
- **osm**: обработка OSM-файла, а именно:
  - parsing - функции, связанные с парсингом xml-файла.
  - processing - обработка данных, полученных модулем parsing.
  - saving_backup - сохранение обработанных данных о городе.  Данные хранятся в папке resources, которая не включена в этот репозиторий.
  - opening_backup - загрузка сериализованных данных из resources.
- **vis**: визуализация данных и все, что с ней связано.
- **pathfinding**: реализация алгоритмов Дейкстры, Левита и А* (все ручной работы)
- **tsp**: реализация **nearest neighbor** и **insertion heuristic** для TSP
- **task2 / task3**: функции, специфические для заданий 2 и 3 

## Результаты
Все промежуточные результаты (кроме 1 задания - его файлы в корневой папке) можно найти в папке out. Более подробная информация в отчетах.
- **Задание 1. Подготовка графа города**
  - adjacency-list.csv + visualisation.svg.
- **Задание 2. Поиск кратчайших путей**
  - Отчет: [задание 2](./out/task2.md). 
- **Задание 3. Задача коммивояжера**
  - Отчет: [задание 3](./out/task3.md)

## А еще
Есть [презентация](https://goo.gl/zFERve) про то, как все это делалось



