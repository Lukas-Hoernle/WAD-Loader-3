package com.luma.wadloader3.ddd1infrastructure.zipper;

import com.luma.wadloader3.ddd1infrastructure.config.WadDir;
import com.luma.wadloader3.ddd3domain.files.services.FileZipper;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@Service
@RequiredArgsConstructor
public class FileZipperImplementation implements FileZipper {

    private final WadDir wadDir;

    /**
     * @param files relative paths based on a common root
     * @return Path to the zip archive
     */
    @Override
    public Failable<Path> zipFiles(List<FileToZip> files) {
        Path newArchivePath = wadDir.zipDirPath().resolve("%s.zip".formatted(UUID.randomUUID()))
                .toAbsolutePath();

        if (!newArchivePath.getParent().toFile().exists()) {
            newArchivePath.getParent().toFile().mkdirs();
        }

        try (FileOutputStream fos = new FileOutputStream(newArchivePath.toString());
             ZipOutputStream zip = new ZipOutputStream(fos)) {

            Failable<Void> zipResult = files.stream()
                    .map(f -> zipAdd(zip, f))
                    // combine all the results to receive all failures
                    .reduce(Failable.success(null), (acc, z) -> acc.combine(z, (a, b) -> b));

            return zipResult.chain(() -> newArchivePath);

        } catch (FileNotFoundException e) {
            return Failable.failure("File not found: " + e.getMessage());
        } catch (IOException e) {
            return Failable.failure("IOException: " + e.getMessage());
        }
    }

    /**
     * @param zip  zip stream.
     * @param file the file to add to the zip stream.
     * @return {@link Failable} indicating weather the computation was successful or not.
     * Because zip is mutated, no useful return value is given.
     */
    private Failable<Void> zipAdd(ZipOutputStream zip, FileToZip file) {
        try (FileInputStream fis = new FileInputStream(file.path().toFile())) {

            ZipEntry zipEntry = new ZipEntry(String.valueOf(file.name()));
            zip.putNextEntry(zipEntry);

            //write 1024 Bytes at once
            //this way there is no need to read the whole file at once
            byte[] bytes = new byte[1024];
            int length;
            while ((length = fis.read(bytes)) >= 0) {
                zip.write(bytes, 0, length);
            }

            return Failable.success(null);

        } catch (FileNotFoundException e) {
            return Failable.failure("File not found: " + e.getMessage());
        } catch (IOException e) {
            return Failable.failure("IOException: " + e.getMessage());
        }
    }



}
