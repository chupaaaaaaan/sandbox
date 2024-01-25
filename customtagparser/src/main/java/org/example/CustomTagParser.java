package org.example;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.nodeTypes.NodeWithSimpleName;
import com.github.javaparser.ast.visitor.VoidVisitorAdapter;
import com.github.javaparser.utils.Log;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class CustomTagParser {


    private final InfoBag infoBag;

    public CustomTagParser(Set<String> attributes, Map<String, InfoBag.TagInfo> tagsInfo) {
        infoBag = new InfoBag(attributes, tagsInfo);
    }

    public void parse(Path tagSource) {
        Log.setAdapter(new Log.StandardOutStandardErrorAdapter());

        CompilationUnit compilationUnit;
        try {
            compilationUnit = StaticJavaParser.parse(tagSource);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        compilationUnit.accept(new TagInfoVisitor(), infoBag);
    }


    public InfoBag.TagInfo walkExtended(String tagName) {

        Map<String, InfoBag.TagInfo> tagsInfo = infoBag.getTagsInfo();
        if(!tagsInfo.containsKey(tagName)) {
            return null;
        }

        InfoBag.TagInfo tagInfo = tagsInfo.get(tagName);

        for(String extendedClassName : tagInfo.getExtendedClasses()) {
            InfoBag.TagInfo extendedClass = this.walkExtended(extendedClassName);
            if(extendedClass != null) {
                tagInfo.merge(extendedClass);
            }
        }

        return tagInfo;
    }


    private static class TagInfoVisitor extends VoidVisitorAdapter<InfoBag> {

        @Override
        public void visit(ClassOrInterfaceDeclaration n, InfoBag infoBag) {

            String tagName = n.getNameAsString();
            Map<String, InfoBag.TagInfo> tagsInfo = infoBag.getTagsInfo();

            if (tagsInfo.containsKey(tagName)) {
                Log.info("Tag already registered. tagName=" + tagName);
                return;
            }

            // 処理対象のタグ情報を格納するインスタンスをバッグに追加する。
            infoBag.setCurrentTagInfo(tagName);
            Log.info("tagName=" + tagName);

            List<String> extendedClassList = n.getExtendedTypes().stream().map(NodeWithSimpleName::getNameAsString).collect(Collectors.toUnmodifiableList());
            infoBag.getCurrentTagInfo().getExtendedClasses().addAll(extendedClassList);

            super.visit(n, infoBag);

            // 解析完了したタグ情報を、バッグのタグ情報マップに移し替える。
            infoBag.registerAndRemoveFixedTagInfo();
        }

        @Override
        public void visit(MethodDeclaration n, InfoBag infoBag) {

            Set<String> attrInfo = infoBag.getAttributes();

            String methodName = n.getNameAsString();
            if (!methodName.startsWith("set")) {
                Log.info("This method is not attribute setter. methodName=" + methodName);
                return;
            }

            InfoBag.TagInfo tagInfo = infoBag.getCurrentTagInfo();

            String attrName = methodName.substring("set".length());
            String attrType = n.getParameters().getFirst().orElseThrow().getTypeAsString();

            if (!tagInfo.getAttrInfo().containsKey(attrName)) {
                tagInfo.getAttrInfo().put(attrName, attrType);
//                Log.info("attrName=" + attrName + "; attrType=" + attrType);
            } else {
                Log.info("Attribute already registered. attrName=" + attrName + "; attrType=" + attrType);
            }

            attrInfo.add(attrName);

            super.visit(n, infoBag);


        }


    }


}
