type WadpackOption = "DownloadAndStartWadPack" | "DownloadWadPack";
/**
 * only works on positive integers
 */
type WadpackId = number;
/**
 * only works on positive integers
 */
type WadId = number;

export function useLocalHandler(serverUrl: string) {
  return (option: WadpackOption, packId: WadpackId, wads: [WadId]) =>
    window.open(`wadloader://${option}${packId}-${wads.join('-')}$${serverUrl}`);
}
