package tokyo.chupaaaaaaan.slack.model;

public class BasicChannel {

    private String name;

    private int numOfMembers;

    public BasicChannel(String name, int numOfMembers) {
        this.name = name;
        this.numOfMembers = numOfMembers;
    }

    public String getName() {
        return name;
    }

    public int getNumOfMembers() {
        return numOfMembers;
    }

    @Override
    public String toString() {
        return this.getName();
    }
}
