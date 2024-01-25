package org.example;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;

public class CustomTagPrettyPrinter {

    private final Set<String> attributes;

    private final Map<String, InfoBag.TagInfo> tagsInfo;


    public CustomTagPrettyPrinter(Set<String> attributes, Map<String, InfoBag.TagInfo> tagsInfo) {
        this.attributes = attributes;
        this.tagsInfo = tagsInfo;
    }

    public void print(Path outputFilePath) {

        try (final BufferedWriter bw = Files.newBufferedWriter(outputFilePath, StandardCharsets.UTF_8, StandardOpenOption.CREATE);
             final PrintWriter pw = new PrintWriter(bw, false)) {

            StringJoiner header = new StringJoiner("\t");
            for (Map.Entry<String, InfoBag.TagInfo> tagInfo : tagsInfo.entrySet()) {
                if(tagInfo.getKey().endsWith("Tag")) {
                    header.add(tagInfo.getKey());
                }
            }
            pw.println("\t" + header);

            for (String attribute : attributes) {
                StringJoiner body = new StringJoiner("\t");
                body.add(attribute);
                for (Map.Entry<String, InfoBag.TagInfo> tagInfo : tagsInfo.entrySet()) {
                    if(tagInfo.getKey().endsWith("Tag")) {
                        body.add(tagInfo.getValue().getAttrInfo().getOrDefault(attribute, "-"));
                    }
                }
                pw.println(body);
            }


        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }


}
