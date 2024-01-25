package org.example;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;


public class App {
    /**
     * @param args コマンドライン引数
     *             1. カスタムタグソースコードが存在するディレクトリ・パス
     *             2. 一覧出力先ファイルパス
     */
    public static void main(String[] args) {

        Path dirPath;
        try {
            dirPath = Paths.get(args[0]).toAbsolutePath().toRealPath(LinkOption.NOFOLLOW_LINKS);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        System.out.println(dirPath);

        PathMatcher pathMatcher = dirPath.getFileSystem().getPathMatcher("glob:**/tag/*.java");

        final Set<String> attributes = new HashSet<>();
        final Map<String, InfoBag.TagInfo> tagsInfo = new HashMap<>();
        CustomTagParser customTagParser = new CustomTagParser(attributes, tagsInfo);

        try (Stream<Path> pathStream = Files.list(dirPath).filter(pathMatcher::matches)) {

            // まず、カスタムタグのソースをすべてパースする。
            pathStream.forEach(customTagParser::parse);

            // 次に、継承したタグの属性をすべて追加する。
            tagsInfo.keySet().forEach(customTagParser::walkExtended);

//            tagsInfo.values().forEach(tagInfo -> {
//                System.out.println("=========================== " + tagInfo.getTagName() + " ===========================");
//                System.out.println(tagInfo.getExtendedClasses());
//                System.out.println(tagInfo.getAttrInfo());
//            });

        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }


        (new CustomTagPrettyPrinter(attributes, tagsInfo))
            .print(Paths.get(args[1]).toAbsolutePath());

    }
}
