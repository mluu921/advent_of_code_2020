library(tidyverse)

## load data 
load_data <- function() {
  tibble::tribble(
    ~values,
    1472L,
    1757L,
    1404L,
    1663L,
    1365L,
    1974L,
    1649L,
    1489L,
    1795L,
    1821L,
    1858L,
    1941L,
    1943L,
    1634L,
    1485L,
    1838L,
    817L,
    1815L,
    1442L,
    639L,
    1182L,
    1632L,
    1587L,
    1918L,
    1040L,
    1441L,
    1784L,
    1725L,
    1951L,
    1285L,
    285L,
    1224L,
    1755L,
    1748L,
    1488L,
    1374L,
    1946L,
    1771L,
    1809L,
    1929L,
    1621L,
    1462L,
    2001L,
    1588L,
    1888L,
    1959L,
    1787L,
    1690L,
    1363L,
    1567L,
    1853L,
    1990L,
    1819L,
    1904L,
    1458L,
    1882L,
    1348L,
    1957L,
    1454L,
    1557L,
    1471L,
    332L,
    1805L,
    1826L,
    1745L,
    1154L,
    1423L,
    1852L,
    1751L,
    1194L,
    1430L,
    1849L,
    1962L,
    1577L,
    1470L,
    1509L,
    1673L,
    1883L,
    1479L,
    1487L,
    2007L,
    1555L,
    1504L,
    1570L,
    2004L,
    978L,
    1681L,
    1631L,
    1791L,
    1267L,
    1245L,
    1383L,
    1482L,
    1355L,
    1792L,
    1806L,
    1376L,
    1199L,
    1391L,
    1759L,
    1474L,
    1268L,
    1942L,
    1936L,
    1766L,
    1233L,
    1876L,
    1674L,
    1761L,
    1542L,
    1468L,
    1543L,
    1986L,
    2005L,
    1689L,
    1606L,
    1865L,
    1783L,
    1807L,
    1779L,
    1860L,
    1408L,
    1505L,
    1435L,
    1205L,
    1952L,
    1201L,
    1714L,
    1743L,
    1872L,
    1897L,
    1978L,
    1683L,
    1846L,
    858L,
    1528L,
    1629L,
    1510L,
    1446L,
    1869L,
    1347L,
    685L,
    1478L,
    1387L,
    687L,
    1964L,
    1968L,
    1429L,
    1460L,
    1777L,
    1417L,
    1768L,
    1672L,
    1767L,
    1400L,
    1914L,
    1715L,
    1425L,
    1700L,
    1756L,
    1835L,
    1926L,
    1889L,
    1568L,
    1393L,
    1960L,
    1540L,
    1810L,
    1401L,
    1685L,
    830L,
    1789L,
    1652L,
    1899L,
    796L,
    1483L,
    1261L,
    1398L,
    1727L,
    1566L,
    1812L,
    1937L,
    1993L,
    1286L,
    1992L,
    1611L,
    1825L,
    1868L,
    1870L,
    1746L,
    1361L,
    1418L,
    1820L,
    1598L,
    1911L,
    1428L,
    1734L,
    1833L,
    1436L,
    1560L
  )
  
}

data <- load_data()


# part1 -------------------------------------------------------------------

## generate the combinations 
temp <- combn(data$values, 2, simplify = T) %>% as_tibble()

## replace with tibble
temp <- tibble(
  x = temp[1,] %>% unlist(),
  y = temp[2,] %>% unlist()
)

## calculate the sum and answer
temp <- temp %>%
  mutate(
    z = x + y,
    answer = x * y
  )

## find correct answer 2020

temp %>%
  filter(
    z == 2020
  )



# part2 -------------------------------------------------------------------

## generate the combinations 
temp <- combn(data$values, 3, simplify = T) %>% as_tibble()

## replace with tibble
temp <- tibble(
  x = temp[1,] %>% unlist(),
  y = temp[2,] %>% unlist(),
  z = temp[3,] %>% unlist()
)

## calculate the sum and answer
temp <- temp %>%
  mutate(
    i = x + y + z,
    answer = x * y * z
  )

## find correct answer 2020

temp %>%
  filter(
    i == 2020
  )
