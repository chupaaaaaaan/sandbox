package tokyo.chupaaaaaaan.toy.slack.app;

/**
 * アプリケーションの実行に必要なパラメータの定義。
 *
 * @param channelId 投稿先のチャネルID
 * @param channelNamePattern 取得するチャネル名の正規表現
 * @param maxRankingCount 表示するランキングの件数
 * @param title メッセージタイトル
 * @param rowPattern 行のパターン
 */
public record AppParams(
        String channelId,
        String channelNamePattern,
        long maxRankingCount,
        String title,
        String rowPattern) {
}
