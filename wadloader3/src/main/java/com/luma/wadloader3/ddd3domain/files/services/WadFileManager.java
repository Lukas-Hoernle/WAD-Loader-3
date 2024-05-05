package com.luma.wadloader3.ddd3domain.files.services;

import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import com.luma.wadloader3.ddd4abstraction.functional.ErrorMessage;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;

import java.io.InputStream;

/**
 * FileManager for WadFiles.
 */
public interface WadFileManager {
    /**
     * @param wadName the name of the wad
     * @param wadFile the wad as a file
     * @return {@link Failable}<{@link ErrorMessage}, {@link FilePath}> containing either the path to the saved file as
     * {@link Failable.Failure} or the reason why the file could not be saved as {@link Failable.Success}.
     */
    Failable<FilePath> saveFile(String wadName, InputStream wadFile);

    /**
     * @param name the name of the wad/file to get the path for.
     * @return {@link Failable}<{@link ErrorMessage}, {@link FilePath}> containing either the path to the file as
     * {@link Failable.Failure} or the reason why the file could not be found as {@link Failable.Success}.
     */
    Failable<FilePath> findFileByName(String name);
}
