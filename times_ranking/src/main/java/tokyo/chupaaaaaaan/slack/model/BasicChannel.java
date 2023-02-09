package tokyo.chupaaaaaaan.slack.model;

public class BasicChannel {

    private String id;

    private String name;

    private int numOfMembers;

    public BasicChannel(String id, String name, int numOfMembers) {
        this.id = id;
        this.name = name;
        this.numOfMembers = numOfMembers;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }
}
