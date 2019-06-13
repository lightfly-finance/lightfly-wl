## Mathematica Client for lightfly

### 安装

打开 `Mathematica` 程序，在顶部菜单栏打开 `文件` -> `安装`, 安装
项目类型选择 `程序包`, 在文件系统中选择下载的程序包。

### 使用

```
<< Lightfly`

data = Stock["hs300", {"app_id" -> "xxx", "secret_key" -> "xxx"}]
```

完整文档： https://www.yuque.com/twn39/bb3s7k