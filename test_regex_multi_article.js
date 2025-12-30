const STOCK_INFO_REGEX = /<WWKS[^>]*?TimeStamp=["'](?<timestamp>[^"']*)["'][^>]*?>.*?<StockInfoMessage[^>]*?Id=["'](?<message_id>[^"']*)["'][^>]*?Source=["'](?<source>[^"']*)["'][^>]*?Destination=["'](?<destination>[^"']*)["'][^>]*?>.*?<Article[^>]*?Id=["'](?<article_id>[^"']*)["'][^>]*?Name=["'](?<article_name>[^"']*)["'][^>]*?DosageForm=["'](?<dosage_form>[^"']*)["'][^>]*?MaxSubItemQuantity=["'](?<max_sub_item_quantity>[^"']*)["'][^>]*?Quantity=["'](?<quantity>[^"']*)["'][^>]*?>.*?<\/StockInfoMessage>.*?<\/WWKS>/sgi;

const multiArticleXml = `
<WWKS TimeStamp="2024-01-01T12:00:00Z">
    <StockInfoMessage Id="100" Source="SystemA" Destination="SystemB">
        <Article Id="A1" Name="Article One" DosageForm="Pill" MaxSubItemQuantity="10" Quantity="50" />
        <Article Id="A2" Name="Article Two" DosageForm="Syrup" MaxSubItemQuantity="1" Quantity="20" />
        <Article Id="A3" Name="Article Three" DosageForm="Cream" MaxSubItemQuantity="1" Quantity="30" />
    </StockInfoMessage>
</WWKS>
`;

console.log("Testing Multi-Article XML:");
let matchCount = 0;
// Reset regex
STOCK_INFO_REGEX.lastIndex = 0;
for (const match of multiArticleXml.matchAll(STOCK_INFO_REGEX)) {
    matchCount++;
    console.log(`Match ${matchCount}:`);
    console.log(`  Timestamp: ${match.groups.timestamp}`);
    console.log(`  Article: ${match.groups.article_name} (ID: ${match.groups.article_id})`);
}

if (matchCount === 0) {
    console.log("No matches found.");
} else if (matchCount === 1) {
    console.log("Found 1 match (Expected 3 Article entries). The regex only captures the first article.");
} else {
    console.log(`Found ${matchCount} matches.`);
}
