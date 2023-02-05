# haskell-stack-buildbase

docker上でstack projectをビルドするためのベースコンテナイメージを作成します。

FPComplete社が提供するstackコンテナイメージに、自分の環境で必要なパッケージを予めインストールします。
このコンテナイメージをstack projectビルド時のベースイメージとして使用することで、依存パッケージのビルド時間を短縮します
（コンテナイメージに未インストールのパッケージは、引き続き都度ビルドが必要です）。
必要に応じて、`package.yaml`に依存パッケージを追加します。

作成したコンテナイメージは、対応するLTSバージョンのstack projectのビルドに使用できます
（stack projectごとにベースイメージを作成する必要はありません）。

## 使用方法

```
./build.bash <Stack-Yaml file>
```

Example:
```
$ ./build.bash stack-lts-20.9.yaml
$ docker image ls
REPOSITORY                TAG        IMAGE ID       CREATED          SIZE
haskell-stack-buildbase   lts-20.9   af238f3d4e00   26 minutes ago   3.06GB
...(omitted)...
```
