package org.example;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class InfoBag {

    private final Set<String> attributes;

    private final Map<String, TagInfo> tagsInfo;

    private final Deque<TagInfo> currentTagInfo = new ArrayDeque<>();

    public InfoBag(Set<String> attributes, Map<String, TagInfo> tagsInfo) {
        this.attributes = attributes;
        this.tagsInfo = tagsInfo;
    }

    public Set<String> getAttributes() {
        return attributes;
    }

    public Map<String, TagInfo> getTagsInfo() {
        return tagsInfo;
    }

    public TagInfo getCurrentTagInfo() {
        return currentTagInfo.getFirst();
    }

    public void setCurrentTagInfo(String tagName) {
        currentTagInfo.addFirst(new TagInfo(tagName));
    }

    public void registerAndRemoveFixedTagInfo() {
        TagInfo cti = currentTagInfo.removeFirst();
//        tagsInfo.put(cti.tagName, TagInfo.fixedOf(cti));
        tagsInfo.put(cti.tagName, cti);
    }

    public static class TagInfo {

        private final String tagName;

        private final Map<String, String> attrInfo;

        private final List<String> extendedClasses;

        private TagInfo(String tagName, Map<String, String> attrInfo, List<String> extendedClasses) {
            this.tagName = tagName;
            this.attrInfo = attrInfo;
            this.extendedClasses = extendedClasses;
        }

        public TagInfo(String tagName) {
            this(tagName, new LinkedHashMap<>(), new ArrayList<>());
        }

        public String getTagName() {
            return tagName;
        }

        public Map<String, String> getAttrInfo() {
            return attrInfo;
        }

        public List<String> getExtendedClasses() {
            return extendedClasses;
        }

        public static TagInfo fixedOf(TagInfo current) {
            return new TagInfo(current.tagName, Map.copyOf(current.attrInfo), List.copyOf(current.extendedClasses));
        }

        public void merge(TagInfo superTag) {
            for(Map.Entry<String, String> attrEntry : superTag.attrInfo.entrySet()) {
                this.attrInfo.putIfAbsent(attrEntry.getKey(), attrEntry.getValue());
            }
        }

    }

}
