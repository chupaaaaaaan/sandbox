package tokyo.chpn.office2text.filter;

import tokyo.chpn.office2text.extract.content.Greppable;

public record Filtered(Condition condition, Greppable greppable) {
}
